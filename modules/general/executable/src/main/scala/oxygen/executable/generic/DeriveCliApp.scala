package oxygen.executable.generic

import oxygen.executable.*
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

/**
  * v2 derivation: builds a [[CompiledCliApp]] from a user's `CliApp`, materialized **per file** as a
  * [[CliApp.Derived]] (`given CliApp.Derived[Foo, R] = CliApp.derive`).
  *
  * Each app derives itself where its own defaults / commands are local; a sub-app command references the
  * child's `Derived` (summoned) instead of recursively reflecting into the child class. This avoids the
  * cross-file default-getter hazard and keeps the macro non-recursive.
  *
  * `R` (the app's `RequiredEnv`) is an explicit type parameter so env composes at every nesting site.
  * `FullEnv` is expressed as `R & ProvidedEnv` so `prependLayer`'s `(R & ProvidedEnv) =:= RIn` is reflexive.
  */
private[executable] object DeriveCliApp {

  def derive[A: Type, R: Type](using Quotes): Expr[CliApp.Derived[A, R]] = {
    // isRoot = false: constructor params are never parsed from the CLI — a root is zero-arg (built by the
    // runner) and a sub-app is built by its parent command. Avoids requiring ctor params to be annotated.
    val repr: RawCliAppRepr[A] = new RawCliAppRepr[A](isRoot = false)

    if !(repr.requiredEnvTypeRepr =:= TypeRepr.of[R]) then
      report.errorAndAbort(
        s"CliApp.derive: declared R ${TypeRepr.of[R].showAnsiCode} does not match this app's RequiredEnv ${repr.requiredEnvTypeRepr.showAnsiCode}",
        repr.gen.pos,
      )

    repr.providedEnvTypeRepr.asTypeOf match
      case '[aProv] =>
        val bodyExpr: Expr[CompiledCliApp[A, R]] = buildApp[A, R, aProv](repr)
        val appExpr: Expr[CompiledCliApp[Unit, R]] = buildRootApp[A, R](repr, bodyExpr)
        '{
          new CliApp.Derived[A, R] {
            override def body: CompiledCliApp[A, R] = $bodyExpr
            override def app: CompiledCliApp[Unit, R] = $appExpr
          }
        }
      case _ => report.errorAndAbort(s"Unable to derive v2 CLI app: ProvidedEnv ${repr.providedEnvTypeRepr.showAnsiCode}", repr.gen.pos)
  }

  /////// app body = inner (at FullEnv) wrapped with this app's own `def env` (-> R) ////////////////////

  private def buildApp[C: Type, R: Type, ProvidedEnv: Type](repr: RawCliAppRepr[C])(using Quotes): Expr[CompiledCliApp[C, R]] = {
    if !repr.providedIsEmpty && repr.envDef.isEmpty then
      report.errorAndAbort("CliApp declares a ProvidedEnv but has no `def env` to provide it", repr.gen.pos)
    val inner: Expr[CompiledCliApp[C, R & ProvidedEnv]] = buildInner[C, R & ProvidedEnv](repr)
    prependEnv[C, R, ProvidedEnv](repr, inner)
  }

  private def buildInner[C: Type, FullEnv: Type](repr: RawCliAppRepr[C])(using Quotes): Expr[CompiledCliApp[C, FullEnv]] =
    if repr.executeDefs.nonEmpty then
      val defRepr = repr.executeDefs.head
      effectLeaf[C, FullEnv](defRepr.defDef.symbol, defRepr.params, defRepr.defDef.name, docsOf(defRepr))
    else if repr.commandDefs.nonEmpty then
      buildSubCommands[C, FullEnv](repr)
    else
      report.errorAndAbort("a v2 CLI app must have a single @execute or one-or-more @command methods", repr.gen.pos)

  private def buildSubCommands[C: Type, FullEnv: Type](repr: RawCliAppRepr[C])(using Quotes): Expr[CompiledCliApp[C, FullEnv]] = {
    val entries: List[Expr[(String, Lazy[CompiledCliApp[C, FullEnv]])]] =
      repr.commandDefs.map { cmd =>
        val name: String = commandName(cmd)
        val childExpr: Expr[CompiledCliApp[C, FullEnv]] = buildCommandChild[C, FullEnv](repr, cmd)
        '{ (${ Expr(name) }, Lazy($childExpr)) }
      }
    val entriesExpr: Expr[List[(String, Lazy[CompiledCliApp[C, FullEnv]])]] = Expr.ofList(entries)
    '{
      new CompiledCliApp.SubCommands[C, FullEnv] {
        override val subCommands: Seq[(String, Lazy[CompiledCliApp[C, FullEnv]])] = $entriesExpr
      }
    }
  }

  private def buildCommandChild[C: Type, FullEnv: Type](parentRepr: RawCliAppRepr[C], cmd: RawDefRepr)(using Quotes): Expr[CompiledCliApp[C, FullEnv]] =
    if cmd.returnTypeRepr <:< parentRepr.effectTypeRepr then
      effectLeaf[C, FullEnv](cmd.defDef.symbol, cmd.params, cmd.defDef.name, docsOf(cmd))
    else
      // Sub-app command: reference the child's own derivation (summoned, not reflected into).
      cmd.returnTypeRepr.asTypeOf match
        case '[child] =>
          childRequiredEnv(cmd).asTypeOf match
            case '[childReq] =>
              val childDerived: Expr[CliApp.Derived[child, childReq]] =
                Expr.summon[CliApp.Derived[child, childReq]].getOrElse {
                  report.errorAndAbort(
                    s"Missing `given CliApp.Derived[${TypeRepr.of[child].typeSymbol.name}, ${TypeRepr.of[childReq].showAnsiCode}]` " +
                      s"— add `given CliApp.Derived[${TypeRepr.of[child].typeSymbol.name}, ...] = CliApp.derive` to its companion",
                    cmd.defPosition,
                  )
                }
              val cmdParamTypeReprs: List[TypeRepr] = cmd.params.map(_.typeRepr)
              val cmdParserExpr: Expr[ExecutableParser[List[Any]]] =
                ParserCodegen.combineToList(cmd.params.map(p => ParserCodegen.buildParamParser(p, cmd.defDef.name)))
              '{
                $childDerived.body.select[C, List[Any]]($cmdParserExpr) { (c, args) =>
                  val _ = args
                  ${ ParserCodegen.callMethod('c, cmd.defDef.symbol, ParserCodegen.typedListArgs('args, cmdParamTypeReprs)).asExprOf[child] }
                }
              }.asExprOf[CompiledCliApp[C, FullEnv]]
            case _ => report.errorAndAbort(s"Unable to derive nested sub-app for ${cmd.returnTypeRepr.showAnsiCode}", cmd.defPosition)
        case _ => report.errorAndAbort(s"Unable to derive nested sub-app for ${cmd.returnTypeRepr.showAnsiCode}", cmd.defPosition)

  // The `RequiredEnv` type arg of a sub-app's `CliApp[RequiredEnv, ProvidedEnv]` (read from the type only).
  private def childRequiredEnv(cmd: RawDefRepr)(using Quotes): TypeRepr =
    cmd.returnTypeRepr.baseType(TypeRepr.of[CliApp[?, ?]].typeSymbol) match
      case AppliedType(_, req :: _ :: Nil) => req
      case _                               => report.errorAndAbort(s"sub-app must extend CliApp[_, _], found ${cmd.returnTypeRepr.showAnsiCode}", cmd.defPosition)

  /////// leaf effect ///////////////////////////////////////////////////////////////////////////////

  private def effectLeaf[C: Type, FullEnv: Type](
      methodSym: oxygen.quoted.Symbol,
      params: List[RawParamRepr],
      ownerName: String,
      docs: List[String],
  )(using Quotes): Expr[CompiledCliApp[C, FullEnv]] = {
    val paramTypeReprs: List[TypeRepr] = params.map(_.typeRepr)
    val parserExpr: Expr[ExecutableParser[List[Any]]] =
      ParserCodegen.combineToList(params.map(p => ParserCodegen.buildParamParser(p, ownerName)))
    '{
      new CompiledCliApp.Effect[C, List[Any], FullEnv] {
        override def commandDocs: List[String] = ${ Expr(docs) }
        override lazy val effectParser: ExecutableParser[List[Any]] = $parserExpr
        override def effect(instances: C, effectArgs: List[Any]): ZIO[FullEnv, ExecutableError, ExitCode] =
          ${ effectBody[C, FullEnv]('instances, 'effectArgs, methodSym, paramTypeReprs) }
      }
    }
  }

  private def docsOf(defRepr: RawDefRepr): List[String] = defRepr.annot_doc.fold(Nil)(_.parts)

  private def effectBody[C: Type, FullEnv: Type](
      instExpr: Expr[C],
      argsExpr: Expr[List[Any]],
      methodSym: oxygen.quoted.Symbol,
      paramTypeReprs: List[TypeRepr],
  )(using Quotes): Expr[ZIO[FullEnv, ExecutableError, ExitCode]] = {
    val typedArgs: List[Expr[?]] = ParserCodegen.typedListArgs(argsExpr, paramTypeReprs)
    val callExpr: Expr[RIO[FullEnv & Scope, Unit | ExitCode]] =
      ParserCodegen.callMethod(instExpr, methodSym, typedArgs).asExprOf[RIO[FullEnv & Scope, Unit | ExitCode]]
    '{
      val _ = $argsExpr // referenced even when the method takes no params (keeps -Werror happy)
      ZIO.scoped[FullEnv]($callExpr).map {
        case ()           => ExitCode.success
        case ec: ExitCode => ec
      }.orDie
    }
  }

  /////// this app's own env layer ////////////////////////////////////////////////////////////////////

  private def prependEnv[C: Type, R: Type, ProvidedEnv: Type](
      repr: RawCliAppRepr[C],
      inner: Expr[CompiledCliApp[C, R & ProvidedEnv]],
  )(using Quotes): Expr[CompiledCliApp[C, R]] =
    repr.envDef match
      case None =>
        // No env => ProvidedEnv = Any, so `R & Any =:= R`: a safe coercion.
        inner.asExprOf[CompiledCliApp[C, R]]
      case Some(envDef) =>
        val tagExpr: Expr[EnvironmentTag[ProvidedEnv]] =
          Expr.summon[EnvironmentTag[ProvidedEnv]].getOrElse {
            report.errorAndAbort(s"Missing EnvironmentTag for ProvidedEnv ${TypeRepr.of[ProvidedEnv].showAnsiCode}", repr.gen.pos)
          }
        val envSym: oxygen.quoted.Symbol = envDef.defDef.symbol
        val envParamTypeReprs: List[TypeRepr] = envDef.params.map(_.typeRepr)
        val envParserExpr: Expr[ExecutableParser[List[Any]]] =
          ParserCodegen.combineToList(envDef.params.map(p => ParserCodegen.buildParamParser(p, "env")))

        def layerBody(instExpr: Expr[C], argsExpr: Expr[List[Any]]): Expr[ZLayer[R, ExecutableError, ProvidedEnv]] = {
          val typedArgs: List[Expr[?]] = ParserCodegen.typedListArgs(argsExpr, envParamTypeReprs)
          val envCall: Expr[ZLayer[R, Throwable, ProvidedEnv]] =
            ParserCodegen.callMethod(instExpr, envSym, typedArgs).asExprOf[ZLayer[R, Throwable, ProvidedEnv]]
          '{
            val _ = $argsExpr // referenced even when `def env` takes no params
            $envCall.orDie
          }
        }

        '{
          given EnvironmentTag[ProvidedEnv] = $tagExpr
          $inner.prependLayer[List[Any], R, ProvidedEnv]($envParserExpr) { (inst, layerArgs) =>
            ${ layerBody('inst, 'layerArgs) }
          }
        }

  /////// root instance construction (zero-arg only) //////////////////////////////////////////////////

  private def buildRootApp[A: Type, R: Type](repr: RawCliAppRepr[A], bodyExpr: Expr[CompiledCliApp[A, R]])(using Quotes): Expr[CompiledCliApp[Unit, R]] =
    repr.gen match
      case _: ProductGeneric.NoFieldsCaseClassGeneric[A] | _: ProductGeneric.CaseObjectGeneric[A] =>
        val zeroArg: Expr[A] = instantiateZeroArg(repr)
        '{
          $bodyExpr.select[Unit, List[Any]](ExecutableParser.Empty.as(List.empty[Any])) { (_, args) =>
            val _ = args; $zeroArg
          }
        }
      case _ =>
        // Has constructor params => only valid as a sub-command (built by its parent), never run directly.
        val name: String = repr.gen.typeRepr.typeSymbol.name
        '{
          $bodyExpr.select[Unit, List[Any]](ExecutableParser.Empty.as(List.empty[Any])) { (_, args) =>
            val _ = args; scala.sys.error(${ Expr(s"$name must be used as a sub-command, not run directly") })
          }
        }

  private def instantiateZeroArg[A](repr: RawCliAppRepr[A])(using Quotes, Type[A]): Expr[A] =
    repr.gen match
      case gen: ProductGeneric.NoFieldsCaseClassGeneric[A] => gen.instantiate.instance
      case gen: ProductGeneric.CaseObjectGeneric[A]        => gen.instantiate.instance
      case _                                               => report.errorAndAbort(s"v2 CLI app must be a zero-arg or case class, found ${repr.gen.toIndentedString}", repr.gen.pos)

  private def commandName(defRepr: RawDefRepr): String = defRepr.annot match
    case command(name) if name.nonEmpty => name.camelToDash
    case _                              => defRepr.defDef.name.camelToDash

}
