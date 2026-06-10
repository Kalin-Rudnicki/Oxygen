package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.*
import oxygen.meta.k0.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

private[executable] object DeriveCliApp {

  def derive[A: Type](using Quotes): Expr[oxygen.executable.DeriveCliApp.Root[A]] = {
    val repr: RawCliAppRepr[A] = new RawCliAppRepr[A](isRoot = true)
    val instanceExpr: Expr[A] = instantiate(repr)
    val compiledCliAppExpr: Expr[CompiledCliApp[Any]] = buildCompiledCliApp(repr, instanceExpr)

    '{
      new oxygen.executable.DeriveCliApp.Root[A] {
        override val app: CompiledCliApp[Any] = $compiledCliAppExpr
      }
    }
  }

  private def instantiate[A](repr: RawCliAppRepr[A])(using Quotes, Type[A]): Expr[A] =
    repr.gen match
      case gen: ProductGeneric.NoFieldsCaseClassGeneric[A] => gen.instantiate.instance
      case gen: ProductGeneric.CaseObjectGeneric[A]        => gen.instantiate.instance
      case gen                                             => report.errorAndAbort(s"CLI app must be a zero-arg class, found ${gen.toIndentedString}", repr.gen.pos)

  private def buildRootParser[A](repr: RawCliAppRepr[A])(using Quotes): Expr[ArgsParser[?]] = {
    val ctorParams: List[ParamRepr] =
      repr.constructorParams.map(p => ParamRepr.extract(p, repr.typeSymbol.name, repr.defaultSyms))
    ParserCodegen.buildParams(ctorParams, repr.typeSymbol.name, repr.defaultSyms)
  }

  private def commandName(defRepr: RawDefRepr): String = defRepr.annot match
    case command(name) if name.nonEmpty => name
    case _                              => defRepr.defDef.name

  private def asExprOfTypeRepr(expr: Expr[?], typeRepr: TypeRepr)(using Quotes): Expr[?] =
    typeRepr.asTypeOf match
      case '[t] => expr.asExprOf[t]
      case _    => report.errorAndAbort(s"Unable to convert to type ${typeRepr.showAnsiCode}")

  private def buildExecutionBody(
      instanceExpr: Expr[?],
      effectTypeRepr: TypeRepr,
      envLayerTypeRepr: TypeRepr,
      envMethodSym: Option[oxygen.quoted.Symbol],
      cmdMethodSym: oxygen.quoted.Symbol,
      envParamTypeReprs: List[TypeRepr],
      cmdParamTypeReprs: List[TypeRepr],
      envParsedValues: Expr[List[Any]],
      cmdParsedValues: Expr[List[Any]],
  )(using Quotes): Expr[URIO[Scope, ExitCode]] = {
    val envArgExprs: List[Expr[?]] = envParamTypeReprs.zipWithIndex.map { (tpe, idx) =>
      tpe.asTypeOf match
        case '[t] => '{ $envParsedValues(${ Expr(idx) }).asInstanceOf[t] }
        case _    => report.errorAndAbort(s"Unable to convert env param type ${tpe.showAnsiCode}")
    }
    val cmdArgExprs: List[Expr[?]] = cmdParamTypeReprs.zipWithIndex.map { (tpe, idx) =>
      tpe.asTypeOf match
        case '[t] => '{ $cmdParsedValues(${ Expr(idx) }).asInstanceOf[t] }
        case _    => report.errorAndAbort(s"Unable to convert command param type ${tpe.showAnsiCode}")
    }
    val effectExpr: Expr[?] = asExprOfTypeRepr(ParserCodegen.callMethod(instanceExpr, cmdMethodSym, cmdArgExprs), effectTypeRepr)
    envMethodSym match
      case Some(sym) =>
        val envLayerExpr: Expr[?] = asExprOfTypeRepr(ParserCodegen.callMethod(instanceExpr, sym, envArgExprs), envLayerTypeRepr)
        '{ DeriveCliAppRuntime.provideEnvAndRun($envLayerExpr, $effectExpr) }
      case None => '{ DeriveCliAppRuntime.runEffect($effectExpr) }
  }

  private def buildSubAppInstance(
      parentInstanceExpr: Expr[?],
      cmdMethodSym: oxygen.quoted.Symbol,
      cmdParamTypeReprs: List[TypeRepr],
      cmdParsedValues: Expr[List[Any]],
  )(using Quotes): Expr[?] = {
    val cmdArgExprs: List[Expr[?]] = cmdParamTypeReprs.zipWithIndex.map { (tpe, idx) =>
      tpe.asTypeOf match
        case '[t] => '{ $cmdParsedValues(${ Expr(idx) }).asInstanceOf[t] }
        case _    => report.errorAndAbort(s"Unable to convert command param type ${tpe.showAnsiCode}")
    }
    ParserCodegen.callMethod(parentInstanceExpr, cmdMethodSym, cmdArgExprs)
  }

  private def buildParamParsers(
      params: List[ParamRepr],
      ownerName: String,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[List[ArgsParser[?]]] = {
    val parsers: List[Expr[ArgsParser[?]]] = params.map(p => ParserCodegen.buildParam(p, ownerName, defaultSyms))
    if parsers.isEmpty then '{ Nil } else Expr.ofList(parsers)
  }

  private def buildEnvParamParsers(repr: RawCliAppRepr[?])(using Quotes): Expr[List[ArgsParser[?]]] =
    repr.envDef match
      case Some(envDef) => buildParamParsers(envDef.params.map(p => ParamRepr.extract(p, "env", repr.defaultSyms)), "env", repr.defaultSyms)
      case None         => '{ Nil }

  private def buildCmdParamParsers(repr: RawCliAppRepr[?], defRepr: RawDefRepr)(using Quotes): Expr[List[ArgsParser[?]]] =
    buildParamParsers(defRepr.params.map(p => ParamRepr.extract(p, defRepr.defDef.name, repr.defaultSyms)), defRepr.defDef.name, repr.defaultSyms)

  private def buildDisplayParser(parsers: Expr[List[ArgsParser[?]]])(using Quotes): Expr[ArgsParser[?]] =
    '{ $parsers.headOption.getOrElse(ArgsParser.unit: ArgsParser[?]) }

  private def buildHelpParserExprs[A](repr: RawCliAppRepr[A])(using Quotes): List[Expr[ArgsParser[?]]] = {
    val rootParserExpr: Expr[ArgsParser[?]] = buildRootParser(repr)
    val envParsers: List[Expr[ArgsParser[?]]] =
      repr.envDef.fold(Nil)(_.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, "env", repr.defaultSyms), "env", repr.defaultSyms)))
    val cmdParsers: List[Expr[ArgsParser[?]]] =
      repr.executeDefs.headOption.fold(Nil)(defRepr =>
        defRepr.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, defRepr.defDef.name, repr.defaultSyms), defRepr.defDef.name, repr.defaultSyms)),
      )
    rootParserExpr :: envParsers ::: cmdParsers
  }

  private def buildSubCommandHelpParserExprs[A](repr: RawCliAppRepr[A], defRepr: RawDefRepr)(using Quotes): List[Expr[ArgsParser[?]]] = {
    val envParsers: List[Expr[ArgsParser[?]]] =
      repr.envDef.fold(Nil)(_.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, "env", repr.defaultSyms), "env", repr.defaultSyms)))
    val cmdParsers: List[Expr[ArgsParser[?]]] =
      defRepr.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, defRepr.defDef.name, repr.defaultSyms), defRepr.defDef.name, repr.defaultSyms))
    envParsers ::: cmdParsers
  }

  private def buildRunFromArgs[A: Type](
      repr: RawCliAppRepr[A],
      instanceExpr: Expr[A],
      rootParserExpr: Expr[ArgsParser[?]],
      helpParserExpr: Expr[ArgsParser[?]],
      subCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]],
  )(using Quotes): Expr[Args => URIO[Scope, ExitCode]] =
    repr.executeDefs.headOption match
      case Some(defRepr) =>
        val envParserExprs: Expr[List[ArgsParser[?]]] = buildEnvParamParsers(repr)
        val cmdParserExprs: Expr[List[ArgsParser[?]]] = buildCmdParamParsers(repr, defRepr)
        val envParamTypeReprs: List[TypeRepr] = repr.envDef.fold(Nil)(_.params.map(_.typeRepr))
        val cmdParamTypeReprs: List[TypeRepr] = defRepr.params.map(_.typeRepr)
        val methodSym: oxygen.quoted.Symbol = defRepr.defDef.symbol
        val envMethodSym: Option[oxygen.quoted.Symbol] = repr.envDef.map(_.defDef.symbol)
        '{
          (args: Args) =>
            DeriveCliAppRuntime.handleHelpOr(
              args = args,
              helpParser = $helpParserExpr,
              subCommands = Map.empty[String, CompiledCliApp[Any]],
              title = Some("Usage"),
              run = stripped =>
                $rootParserExpr.parseArgs(stripped) match
                  case CliParseResult.Fail(_, help) => DeriveCliAppRuntime.printHelp(help)
                  case CliParseResult.Success(_, remaining) =>
                    DeriveCliAppRuntime.runParsedN(
                      parsers = $envParserExprs,
                      args = remaining,
                      runParsed = { envValues =>
                        DeriveCliAppRuntime.runParsedN(
                          parsers = $cmdParserExprs,
                          args = remaining,
                          runParsed = { cmdValues =>
                            ${ buildExecutionBody(instanceExpr, repr.effectTypeRepr, repr.envLayerTypeRepr, envMethodSym, methodSym, envParamTypeReprs, cmdParamTypeReprs, 'envValues, 'cmdValues) }
                          },
                        )
                      },
                    ),
            )
        }
      case None =>
        '{
          (args: Args) =>
            DeriveCliAppRuntime.runWithSubCommandsFromArgs(
              rootParser = $rootParserExpr,
              helpParser = $helpParserExpr,
              subCommands = $subCommandsExpr,
              args = args,
            )
        }

  private def buildRunFnFromArgs[A: Type](
      repr: RawCliAppRepr[A],
      instanceExpr: Expr[A],
      rootParserExpr: Expr[ArgsParser[?]],
      helpParserExpr: Expr[ArgsParser[?]],
      subCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]],
  )(using Quotes): Expr[List[String] => URIO[Scope, ExitCode]] = {
    val runFromArgsExpr: Expr[Args => URIO[Scope, ExitCode]] =
      buildRunFromArgs(repr, instanceExpr, rootParserExpr, helpParserExpr, subCommandsExpr)
    '{
      (args: List[String]) =>
        Args.parse(args) match
          case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
          case Right(parsedArgs) => $runFromArgsExpr(parsedArgs)
    }
  }

  private def buildCompiledCliApp[A: Type](
      repr: RawCliAppRepr[A],
      instanceExpr: Expr[A],
  )(using Quotes): Expr[CompiledCliApp[Any]] = {
    val rootParserExpr: Expr[ArgsParser[?]] = buildRootParser(repr)
    val helpParserExpr: Expr[ArgsParser[?]] = ParserCodegen.combineHelp(buildHelpParserExprs(repr))
    val subCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]] =
      if repr.commandDefs.nonEmpty then buildSubCommands(repr, instanceExpr)
      else '{ Map.empty[String, CompiledCliApp[Any]] }
    val runFnExpr: Expr[List[String] => URIO[Scope, ExitCode]] =
      buildRunFnFromArgs(repr, instanceExpr, rootParserExpr, helpParserExpr, subCommandsExpr)

    '{
      CompiledCliApp.Impl(
        rootParser = $rootParserExpr,
        helpParser = $helpParserExpr,
        subCommands = $subCommandsExpr,
        runFn = $runFnExpr,
      )
    }
  }

  private def buildRunWithRootFn[A: Type](
      repr: RawCliAppRepr[A],
      instanceExpr: Expr[A],
      envMethodSym: Option[oxygen.quoted.Symbol],
      cmdMethodSym: oxygen.quoted.Symbol,
      envParamTypeReprs: List[TypeRepr],
      cmdParamTypeReprs: List[TypeRepr],
      envParserExprs: Expr[List[ArgsParser[?]]],
      cmdParserExprs: Expr[List[ArgsParser[?]]],
  )(using Quotes): Expr[(Any, Args) => URIO[Scope, ExitCode]] =
    '{
      (_: Any, cmdArgs: Args) =>
        DeriveCliAppRuntime.runParsedN(
          parsers = $envParserExprs,
          args = cmdArgs,
          runParsed = { envValues =>
            DeriveCliAppRuntime.runParsedN(
              parsers = $cmdParserExprs,
              args = cmdArgs,
              runParsed = { cmdValues =>
                ${ buildExecutionBody(instanceExpr, repr.effectTypeRepr, repr.envLayerTypeRepr, envMethodSym, cmdMethodSym, envParamTypeReprs, cmdParamTypeReprs, 'envValues, 'cmdValues) }
              },
            )
          },
        )
    }

  private def buildRunSubAppWithRootFn[A: Type, B: Type](
      parentRepr: RawCliAppRepr[A],
      parentInstanceExpr: Expr[A],
      defRepr: RawDefRepr,
      nestedRepr: RawCliAppRepr[B],
  )(using Quotes): Expr[(Any, Args) => URIO[Scope, ExitCode]] = {
    val envParserExprs: Expr[List[ArgsParser[?]]] = buildEnvParamParsers(parentRepr)
    val cmdParserExprs: Expr[List[ArgsParser[?]]] = buildCmdParamParsers(parentRepr, defRepr)
    val envMethodSym: Option[oxygen.quoted.Symbol] = parentRepr.envDef.map(_.defDef.symbol)
    val cmdMethodSym: oxygen.quoted.Symbol = defRepr.defDef.symbol
    val envParamTypeReprs: List[TypeRepr] = parentRepr.envDef.fold(Nil)(_.params.map(_.typeRepr))
    val cmdParamTypeReprs: List[TypeRepr] = defRepr.params.map(_.typeRepr)
    val nestedRootParserExpr: Expr[ArgsParser[?]] = buildRootParser(nestedRepr)

    '{
      (_: Any, cmdArgs: Args) =>
        DeriveCliAppRuntime.runParsedN(
          parsers = $envParserExprs,
          args = cmdArgs,
          runParsed = { envValues =>
            DeriveCliAppRuntime.runParsedN(
              parsers = $cmdParserExprs,
              args = cmdArgs,
              runParsed = { cmdValues =>
                val subAppInstance = ${
                  buildSubAppInstance(parentInstanceExpr, cmdMethodSym, cmdParamTypeReprs, '{ cmdValues }).asExprOf[B]
                }
                ${
                  val nestedSubCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]] =
                    if nestedRepr.commandDefs.nonEmpty then buildSubCommands(nestedRepr, '{ subAppInstance }.asExprOf[B])
                    else '{ Map.empty[String, CompiledCliApp[Any]] }
                  val nestedHelpParserExpr: Expr[ArgsParser[?]] = ParserCodegen.combineHelp(buildHelpParserExprs(nestedRepr))
                  val nestedRunFromArgsExpr: Expr[Args => URIO[Scope, ExitCode]] =
                    buildRunFromArgs(nestedRepr, '{ subAppInstance }.asExprOf[B], nestedRootParserExpr, nestedHelpParserExpr, nestedSubCommandsExpr)
                  buildRunNestedApp(parentRepr, parentInstanceExpr, envMethodSym, envParamTypeReprs, nestedRunFromArgsExpr, '{ cmdArgs }, '{ envValues })
                }
              },
            )
          },
        )
    }
  }

  private def buildRunNestedApp[A: Type](
      parentRepr: RawCliAppRepr[A],
      parentInstanceExpr: Expr[A],
      envMethodSym: Option[oxygen.quoted.Symbol],
      envParamTypeReprs: List[TypeRepr],
      nestedRunFromArgsExpr: Expr[Args => URIO[Scope, ExitCode]],
      cmdArgsExpr: Expr[Args],
      envParsedValues: Expr[List[Any]],
  )(using Quotes): Expr[URIO[Scope, ExitCode]] =
    envMethodSym match
      case Some(sym) =>
        val envArgExprs: List[Expr[?]] = envParamTypeReprs.zipWithIndex.map { (tpe, idx) =>
          tpe.asTypeOf match
            case '[t] => '{ $envParsedValues(${ Expr(idx) }).asInstanceOf[t] }
            case _    => report.errorAndAbort(s"Unable to convert env param type ${tpe.showAnsiCode}")
        }
        val envLayerExpr: Expr[?] =
          asExprOfTypeRepr(ParserCodegen.callMethod(parentInstanceExpr, sym, envArgExprs), parentRepr.envLayerTypeRepr)
        '{ DeriveCliAppRuntime.provideEnvLayer($envLayerExpr, $nestedRunFromArgsExpr($cmdArgsExpr)) }
      case None =>
        '{ $nestedRunFromArgsExpr($cmdArgsExpr) }

  private def buildRunSubAppStandaloneFn[A: Type, B: Type](
      parentRepr: RawCliAppRepr[A],
      parentInstanceExpr: Expr[A],
      defRepr: RawDefRepr,
      nestedRepr: RawCliAppRepr[B],
  )(using Quotes): Expr[List[String] => URIO[Scope, ExitCode]] = {
    val runWithRootFnExpr: Expr[(Any, Args) => URIO[Scope, ExitCode]] =
      buildRunSubAppWithRootFn(parentRepr, parentInstanceExpr, defRepr, nestedRepr)
    '{
      (args: List[String]) =>
        Args.parse(args) match
          case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
          case Right(parsedArgs) => $runWithRootFnExpr((), parsedArgs)
    }
  }

  private def buildSubCommands[A](repr: RawCliAppRepr[A], instanceExpr: Expr[A])(using Quotes, Type[A]): Expr[Map[String, CompiledCliApp[Any]]] = {
    val envParserExprs: Expr[List[ArgsParser[?]]] = buildEnvParamParsers(repr)
    val envMethodSym: Option[oxygen.quoted.Symbol] = repr.envDef.map(_.defDef.symbol)

    val entries: List[Expr[(String, CompiledCliApp[Any])]] =
      repr.commandDefs.map { defRepr =>
        val name: String = commandName(defRepr)
        val cmdParserExprs: Expr[List[ArgsParser[?]]] = buildCmdParamParsers(repr, defRepr)
        val cmdParserExpr: Expr[ArgsParser[?]] = buildDisplayParser(cmdParserExprs)
        val helpParserExpr: Expr[ArgsParser[?]] = ParserCodegen.combineHelp(buildSubCommandHelpParserExprs(repr, defRepr))
        val methodSym: oxygen.quoted.Symbol = defRepr.defDef.symbol
        val envParamTypeReprs: List[TypeRepr] = repr.envDef.fold(Nil)(_.params.map(_.typeRepr))
        val cmdParamTypeReprs: List[TypeRepr] = defRepr.params.map(_.typeRepr)
        val isEffect: Boolean = defRepr.returnTypeRepr <:< repr.effectTypeRepr
        val runWithRootFnExpr: Expr[(Any, Args) => URIO[Scope, ExitCode]] =
          if isEffect then buildRunWithRootFn(repr, instanceExpr, envMethodSym, methodSym, envParamTypeReprs, cmdParamTypeReprs, envParserExprs, cmdParserExprs)
          else
            defRepr.returnTypeRepr.asTypeOf match
              case '[nestedA] =>
                val nestedRepr = new RawCliAppRepr[nestedA](isRoot = false)
                buildRunSubAppWithRootFn(repr, instanceExpr, defRepr, nestedRepr)
              case _ => report.errorAndAbort(s"Unable to derive nested CLI app for ${defRepr.returnTypeRepr.showAnsiCode}")
        val standaloneRunFnExpr: Expr[List[String] => URIO[Scope, ExitCode]] =
          if isEffect then
            '{
              (args: List[String]) =>
                Args.parse(args) match
                  case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
                  case Right(parsedArgs) =>
                    DeriveCliAppRuntime.runParsedN(
                      parsers = $envParserExprs,
                      args = parsedArgs,
                      runParsed = { envValues =>
                        DeriveCliAppRuntime.runParsedN(
                          parsers = $cmdParserExprs,
                          args = parsedArgs,
                          runParsed = { cmdValues =>
                            ${ buildExecutionBody(instanceExpr, repr.effectTypeRepr, repr.envLayerTypeRepr, envMethodSym, methodSym, envParamTypeReprs, cmdParamTypeReprs, 'envValues, 'cmdValues) }
                          },
                        )
                      },
                    )
            }
          else
            defRepr.returnTypeRepr.asTypeOf match
              case '[nestedA] =>
                val nestedRepr = new RawCliAppRepr[nestedA](isRoot = false)
                buildRunSubAppStandaloneFn(repr, instanceExpr, defRepr, nestedRepr)
              case _ => report.errorAndAbort(s"Unable to derive nested CLI app for ${defRepr.returnTypeRepr.showAnsiCode}")

        '{
          (
            ${ Expr(name) },
            CompiledCliApp.Impl(
              rootParser = $cmdParserExpr,
              helpParser = $helpParserExpr,
              subCommands = Map.empty[String, CompiledCliApp[Any]],
              runFn = $standaloneRunFnExpr,
              runWithRootFn = Some($runWithRootFnExpr),
            ),
          )
        }
      }
    entries match
      case Nil => '{ Map.empty[String, CompiledCliApp[Any]] }
      case nonEmpty =>
        val itemsExpr: Expr[List[(String, CompiledCliApp[Any])]] = Expr.ofList(nonEmpty)
        '{ $itemsExpr.toMap }
  }

}