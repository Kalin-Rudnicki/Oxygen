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
    val compiledCliAppExpr: Expr[CompiledCliApp[Any]] = buildCompiledCliApp(repr)

    '{
      new oxygen.executable.DeriveCliApp.Root[A] {
        override val app: CompiledCliApp[Any] = $compiledCliAppExpr
      }
    }
  }

  private def instantiateZeroArg[A](repr: RawCliAppRepr[A])(using Quotes, Type[A]): Expr[A] =
    repr.gen match
      case gen: ProductGeneric.NoFieldsCaseClassGeneric[A] => gen.instantiate.instance
      case gen: ProductGeneric.CaseObjectGeneric[A]        => gen.instantiate.instance
      case gen                                             => report.errorAndAbort(s"CLI app must be a zero-arg class, found ${gen.toIndentedString}", repr.gen.pos)

  private def instanceFromRootParsed[A](repr: RawCliAppRepr[A], rootParsed: Expr[Any])(using Quotes, Type[A]): Expr[A] =
    if repr.constructorParams.isEmpty then instantiateZeroArg(repr)
    else
      val ctorParamTypeReprs: List[TypeRepr] = repr.constructorParams.map(_.typeRepr)
      val argExprs: List[Expr[?]] = ParserCodegen.typedArgs(rootParsed, ctorParamTypeReprs)
      repr.gen.fieldsToInstance(argExprs).asExprOf[A]

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

  private def docHelpParserExprs(defRepr: RawDefRepr)(using Quotes): List[Expr[ArgsParser[?]]] =
    defRepr.annot_doc.fold(Nil)(doc => List(ParserCodegen.docHelpParser(doc.parts)))

  private def buildHelpParserExprs[A](repr: RawCliAppRepr[A])(using Quotes): List[Expr[ArgsParser[?]]] = {
    val rootParserExpr: Expr[ArgsParser[?]] = buildRootParser(repr)
    val envDocParsers: List[Expr[ArgsParser[?]]] = repr.envDef.fold(Nil)(docHelpParserExprs)
    val envParsers: List[Expr[ArgsParser[?]]] =
      repr.envDef.fold(Nil)(_.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, "env", repr.defaultSyms), "env", repr.defaultSyms)))
    val cmdParsers: List[Expr[ArgsParser[?]]] =
      repr.executeDefs.headOption.fold(List.empty)(defRepr =>
        docHelpParserExprs(defRepr) :::
          defRepr.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, defRepr.defDef.name, repr.defaultSyms), defRepr.defDef.name, repr.defaultSyms)),
      )
    rootParserExpr :: envDocParsers ::: envParsers ::: cmdParsers
  }

  private def buildSubCommandHelpParserExprs[A](repr: RawCliAppRepr[A], defRepr: RawDefRepr)(using Quotes): List[Expr[ArgsParser[?]]] = {
    val envParsers: List[Expr[ArgsParser[?]]] =
      repr.envDef.fold(Nil)(_.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, "env", repr.defaultSyms), "env", repr.defaultSyms)))
    val cmdParsers: List[Expr[ArgsParser[?]]] =
      defRepr.params.map(p => ParserCodegen.buildParam(ParamRepr.extract(p, defRepr.defDef.name, repr.defaultSyms), defRepr.defDef.name, repr.defaultSyms))
    docHelpParserExprs(defRepr) ::: envParsers ::: cmdParsers
  }

  private def buildRunFromArgs[A: Type](
      repr: RawCliAppRepr[A],
      rootParserExpr: Expr[ArgsParser[?]],
      helpParserExpr: Expr[ArgsParser[?]],
      subCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]],
      instanceExpr: Option[Expr[A]] = None,
      commandPathExpr: Expr[List[String]],
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
              commandPath = $commandPathExpr,
              run = stripped =>
                $rootParserExpr.parseArgs(stripped) match
                  case CliParseResult.Fail(_, help) => DeriveCliAppRuntime.printHelp(help)
                  case CliParseResult.Success(rootParsed, remaining) =>
                    DeriveCliAppRuntime.runParsedN(
                      parsers = $envParserExprs,
                      args = remaining,
                      validateRemaining = false,
                      runParsed = { (envValues, afterEnv) =>
                        DeriveCliAppRuntime.runParsedN(
                          parsers = $cmdParserExprs,
                          args = afterEnv,
                          validateRemaining = true,
                          runParsed = { (cmdValues, _) =>
                            val instance = ${ instanceExpr.fold(instanceFromRootParsed(repr, '{ rootParsed }))(identity) }
                            ${ buildExecutionBody('{ instance }, repr.effectTypeRepr, repr.envLayerTypeRepr, envMethodSym, methodSym, envParamTypeReprs, cmdParamTypeReprs, 'envValues, 'cmdValues) }
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
              parsedArgs = args,
              commandPath = $commandPathExpr,
            )
        }

  private def buildRunFnFromArgs[A: Type](
      repr: RawCliAppRepr[A],
      rootParserExpr: Expr[ArgsParser[?]],
      helpParserExpr: Expr[ArgsParser[?]],
      subCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]],
  )(using Quotes): Expr[List[String] => URIO[Scope, ExitCode]] =
    if repr.commandDefs.nonEmpty && repr.executeDefs.isEmpty then
      '{
        (rawArgs: List[String]) =>
          Args.parse(rawArgs) match
            case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
            case Right(parsedArgs) =>
              DeriveCliAppRuntime.runWithSubCommandsFromArgs(
                rootParser = $rootParserExpr,
                helpParser = $helpParserExpr,
                subCommands = $subCommandsExpr,
                parsedArgs = parsedArgs,
                rawArgs = Some(rawArgs),
              )
      }
    else
      val runFromArgsExpr: Expr[Args => URIO[Scope, ExitCode]] =
        buildRunFromArgs(repr, rootParserExpr, helpParserExpr, subCommandsExpr, commandPathExpr = '{ Nil })
      '{
        (rawArgs: List[String]) =>
          Args.parse(rawArgs) match
            case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
            case Right(parsedArgs) => $runFromArgsExpr(parsedArgs)
      }

  private def buildCompiledCliApp[A: Type](
      repr: RawCliAppRepr[A],
  )(using Quotes): Expr[CompiledCliApp[Any]] = {
    val rootParserExpr: Expr[ArgsParser[?]] = buildRootParser(repr)
    val helpParserExpr: Expr[ArgsParser[?]] = ParserCodegen.combineHelp(buildHelpParserExprs(repr))
    val subCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]] =
      if repr.commandDefs.nonEmpty then buildSubCommands(repr)
      else '{ Map.empty[String, CompiledCliApp[Any]] }
    val runFnExpr: Expr[List[String] => URIO[Scope, ExitCode]] =
      buildRunFnFromArgs(repr, rootParserExpr, helpParserExpr, subCommandsExpr)

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
      envMethodSym: Option[oxygen.quoted.Symbol],
      cmdMethodSym: oxygen.quoted.Symbol,
      envParamTypeReprs: List[TypeRepr],
      cmdParamTypeReprs: List[TypeRepr],
      envParserExprs: Expr[List[ArgsParser[?]]],
      cmdParserExprs: Expr[List[ArgsParser[?]]],
  )(using Quotes): Expr[(Any, Args) => URIO[Scope, ExitCode]] =
    '{
      (rootParsed: Any, cmdArgs: Args) =>
        val instance = ${ instanceFromRootParsed(repr, '{ rootParsed }) }
        DeriveCliAppRuntime.runParsedN(
          parsers = $envParserExprs,
          args = cmdArgs,
          validateRemaining = false,
          runParsed = { (envValues, afterEnv) =>
            DeriveCliAppRuntime.runParsedN(
              parsers = $cmdParserExprs,
              args = afterEnv,
              validateRemaining = true,
              runParsed = { (cmdValues, _) =>
                ${ buildExecutionBody('{ instance }, repr.effectTypeRepr, repr.envLayerTypeRepr, envMethodSym, cmdMethodSym, envParamTypeReprs, cmdParamTypeReprs, 'envValues, 'cmdValues) }
              },
            )
          },
        )
    }

  private def buildRunSubAppWithRootFn[A: Type, B: Type](
      parentRepr: RawCliAppRepr[A],
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
    val parentCmdName: String = commandName(defRepr)
    val nestedHasSubCommands: Boolean = nestedRepr.commandDefs.nonEmpty

    '{
      (rootParsed: Any, cmdArgs: Args) =>
        val parentInstance = ${ instanceFromRootParsed(parentRepr, '{ rootParsed }) }
        DeriveCliAppRuntime.runParsedN(
          parsers = $envParserExprs,
          args = cmdArgs,
          validateRemaining = false,
          runParsed = { (envValues, afterEnv) =>
            DeriveCliAppRuntime.runParsedN(
              parsers = $cmdParserExprs,
              args = afterEnv,
              validateRemaining = ${ Expr(!nestedHasSubCommands) },
              runParsed = { (cmdValues, afterCmd) =>
                val subAppInstance = ${
                  buildSubAppInstance('{ parentInstance }, cmdMethodSym, cmdParamTypeReprs, '{ cmdValues }).asExprOf[B]
                }
                ${
                  val nestedSubCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]] =
                    if nestedRepr.commandDefs.nonEmpty then buildSubCommands(nestedRepr)
                    else '{ Map.empty[String, CompiledCliApp[Any]] }
                  val nestedHelpParserExpr: Expr[ArgsParser[?]] = ParserCodegen.combineHelp(buildHelpParserExprs(nestedRepr))
                  val nestedCommandPathExpr: Expr[List[String]] = '{ List(${ Expr(parentCmdName) }) }
                  val nestedRunFromArgsExpr: Expr[Args => URIO[Scope, ExitCode]] =
                    buildRunFromArgs(
                      nestedRepr,
                      nestedRootParserExpr,
                      nestedHelpParserExpr,
                      nestedSubCommandsExpr,
                      Some('{ subAppInstance }.asExprOf[B]),
                      nestedCommandPathExpr,
                    )
                  buildRunNestedApp(parentRepr, '{ parentInstance }, envMethodSym, envParamTypeReprs, nestedRunFromArgsExpr, '{ afterCmd }, '{ envValues })
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
      nestedArgsExpr: Expr[Args],
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
        '{ DeriveCliAppRuntime.provideEnvLayer($envLayerExpr, $nestedRunFromArgsExpr($nestedArgsExpr)) }
      case None =>
        '{ $nestedRunFromArgsExpr($nestedArgsExpr) }

  private def buildStandaloneEffectRunFn[A: Type](
      repr: RawCliAppRepr[A],
      rootParserExpr: Expr[ArgsParser[?]],
      envParserExprs: Expr[List[ArgsParser[?]]],
      cmdParserExprs: Expr[List[ArgsParser[?]]],
      envMethodSym: Option[oxygen.quoted.Symbol],
      methodSym: oxygen.quoted.Symbol,
      envParamTypeReprs: List[TypeRepr],
      cmdParamTypeReprs: List[TypeRepr],
  )(using Quotes): Expr[List[String] => URIO[Scope, ExitCode]] =
    '{
      (args: List[String]) =>
        Args.parse(args) match
          case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
          case Right(parsedArgs) =>
            DeriveCliAppRuntime.runParsedN(
              parsers = ${ if repr.constructorParams.isEmpty then envParserExprs else '{ List($rootParserExpr) ++ $envParserExprs } },
              args = parsedArgs,
              validateRemaining = false,
              runParsed = { (rootAndEnvValues, afterRootAndEnv) =>
                val rootParsed = ${ if repr.constructorParams.isEmpty then '{ () } else '{ rootAndEnvValues.head } }
                val envValues = ${ if repr.constructorParams.isEmpty then '{ rootAndEnvValues } else '{ rootAndEnvValues.tail } }
                val instance = ${ instanceFromRootParsed(repr, '{ rootParsed }) }
                DeriveCliAppRuntime.runParsedN(
                  parsers = $cmdParserExprs,
                  args = afterRootAndEnv,
                  validateRemaining = true,
                  runParsed = { (cmdValues, _) =>
                    ${ buildExecutionBody('{ instance }, repr.effectTypeRepr, repr.envLayerTypeRepr, envMethodSym, methodSym, envParamTypeReprs, cmdParamTypeReprs, '{ envValues }, '{ cmdValues }) }
                  },
                )
              },
            )
    }

  private def buildRunSubAppStandaloneFn[A: Type, B: Type](
      parentRepr: RawCliAppRepr[A],
      defRepr: RawDefRepr,
      nestedRepr: RawCliAppRepr[B],
  )(using Quotes): Expr[List[String] => URIO[Scope, ExitCode]] = {
    val rootParserExpr: Expr[ArgsParser[?]] = buildRootParser(parentRepr)
    val runWithRootFnExpr: Expr[(Any, Args) => URIO[Scope, ExitCode]] =
      buildRunSubAppWithRootFn(parentRepr, defRepr, nestedRepr)
    if parentRepr.constructorParams.isEmpty then
      '{
        (args: List[String]) =>
          Args.parse(args) match
            case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
            case Right(parsedArgs) => $runWithRootFnExpr((), parsedArgs)
      }
    else
      '{
        (args: List[String]) =>
          Args.parse(args) match
            case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
            case Right(parsedArgs) =>
              $rootParserExpr.parseArgs(parsedArgs) match
                case CliParseResult.Fail(_, help) => DeriveCliAppRuntime.printHelp(help)
                case CliParseResult.Success(rootParsed, remaining) => $runWithRootFnExpr(rootParsed, remaining)
      }
  }

  private def buildSubCommands[A](repr: RawCliAppRepr[A])(using Quotes, Type[A]): Expr[Map[String, CompiledCliApp[Any]]] = {
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
        val rootParserExpr: Expr[ArgsParser[?]] = buildRootParser(repr)
        val (runWithRootFnExpr, standaloneRunFnExpr, subCommandsExpr) =
          if isEffect then
            (
              buildRunWithRootFn(repr, envMethodSym, methodSym, envParamTypeReprs, cmdParamTypeReprs, envParserExprs, cmdParserExprs),
              buildStandaloneEffectRunFn(repr, rootParserExpr, envParserExprs, cmdParserExprs, envMethodSym, methodSym, envParamTypeReprs, cmdParamTypeReprs),
              '{ Map.empty[String, CompiledCliApp[Any]] },
            )
          else
            defRepr.returnTypeRepr.asTypeOf match
              case '[nestedA] =>
                val nestedRepr = new RawCliAppRepr[nestedA](isRoot = false)
                val nestedSubCommandsExpr: Expr[Map[String, CompiledCliApp[Any]]] =
                  if nestedRepr.commandDefs.nonEmpty then buildSubCommands(nestedRepr)
                  else '{ Map.empty[String, CompiledCliApp[Any]] }
                (
                  buildRunSubAppWithRootFn(repr, defRepr, nestedRepr),
                  buildRunSubAppStandaloneFn(repr, defRepr, nestedRepr),
                  nestedSubCommandsExpr,
                )
              case _ => report.errorAndAbort(s"Unable to derive nested CLI app for ${defRepr.returnTypeRepr.showAnsiCode}")

        '{
          (
            ${ Expr(name) },
            CompiledCliApp.Impl(
              rootParser = $cmdParserExpr,
              helpParser = $helpParserExpr,
              subCommands = $subCommandsExpr,
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