package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

/**
  * v2 codegen: builds `ExecutableParser` values (CLI via `ArgsParser`, non-CLI via env/config) for
  * the new `CompiledCliApp` pipeline. Unlike the old `ParserCodegen`, this emits calls to the
  * runtime `*.Builder` typeclasses rather than constructing parser AST by hand — the macro only
  * extracts shape. See cli-decisions.md.
  *
  * Each param parser is produced as `ExecutableParser[Any]` (value boxed) so that a list of them can
  * be combined uniformly into `ExecutableParser[List[Any]]` without computing tuple types in the macro.
  */
private[generic] object ParserCodegen {

  private def subHelpExpr(raw: RawParamRepr)(using Quotes): Expr[SubHelp] =
    raw.annot_doc.map(_.parts) match
      case Some(ds) if ds.nonEmpty => '{ SubHelp.fromDocs(${ Expr(ds) }) }
      case _                       => '{ SubHelp.Empty }

  private def longName(raw: RawParamRepr): String =
    raw.annot_longName.fold(raw.valDef.name.camelToDash)(_.name.camelToDash)

  private def shortNameExpr(raw: RawParamRepr)(using Quotes): Expr[Defaultable.Opt[Char]] =
    raw.annot_shortName match
      case None                               => '{ Defaultable.Default }
      case Some(_: oxygen.cli.shortName.none) => '{ Defaultable.Explicit(None) }
      case Some(_: oxygen.cli.shortName.auto) => '{ Defaultable.Default }
      case Some(s: oxygen.cli.shortName)      => '{ Defaultable.Explicit(Some(${ Expr(s.name) })) }

  private def defaultTerm(raw: RawParamRepr, ownerName: String): Option[Term] =
    raw.defaultSyms.get((ownerName, raw.paramIdx))

  // The SCREAMING_SNAKE_CASE auto-derived env var name (cli-decisions.md D3).
  private def screamingSnake(name: String): String =
    name.camelToDash.replace('-', '_').toUpperCase

  /** Build a single param parser, boxed to `ExecutableParser[Any]`. */
  def buildParamParser(raw: RawParamRepr, ownerName: String)(using Quotes): Expr[ExecutableParser[Any]] = {
    val boxed: Expr[ExecutableParser[?]] =
      raw.annot_paramType match
        case positional()    => buildPositional(raw, ownerName)
        case named()         => buildNamed(raw, ownerName)
        case flag()          => buildFlag(raw, ownerName)
        case toggle()        => buildToggle(raw)
        case custom()        => buildCustom(raw)
        case envVar(name)    => buildEnvVar(raw, name, ownerName)
        case envConfig(e, d) => buildEnvConfig(raw, e, d, ownerName)
    '{ $boxed.map(a => (a: Any)) }
  }

  private def buildPositional(raw: RawParamRepr, ownerName: String)(using Quotes): Expr[ExecutableParser[?]] =
    raw.typeRepr.asTypeOf match
      case '[t] =>
        val builder = Implicits.searchRequired[PositionalArgsParser.Builder[t]](
          s"Missing PositionalArgsParser.Builder for ${raw.typeRepr.showAnsiCode}",
          raw.valPosition,
        )
        val base: Expr[PositionalArgsParser[t]] = '{ $builder.build(${ Expr(longName(raw)) }, ${ subHelpExpr(raw) }) }
        val withDef: Expr[PositionalArgsParser[t]] =
          defaultTerm(raw, ownerName) match
            case Some(rhs) => '{ $base.withDefault(${ rhs.asExprOf[t] }) }
            case None      => base
        '{ CLIExecutableParser($withDef) }
      case _ => raw.failAtVal(s"Unable to build positional param for ${raw.typeRepr.showAnsiCode}")

  private def buildNamed(raw: RawParamRepr, ownerName: String)(using Quotes): Expr[ExecutableParser[?]] =
    raw.typeRepr.asTypeOf match
      case '[t] =>
        val builder = Implicits.searchRequired[NamedArgsParser.Builder[t]](
          s"Missing NamedArgsParser.Builder for ${raw.typeRepr.showAnsiCode}",
          raw.valPosition,
        )
        val base: Expr[NamedArgsParser[t]] = '{ $builder.build(${ Expr(longName(raw)) }, ${ shortNameExpr(raw) }, ${ subHelpExpr(raw) }) }
        val withDef: Expr[NamedArgsParser[t]] =
          defaultTerm(raw, ownerName) match
            case Some(rhs) => '{ $base.withDefault(${ rhs.asExprOf[t] }) }
            case None      => base
        '{ CLIExecutableParser($withDef) }
      case _ => raw.failAtVal(s"Unable to build named param for ${raw.typeRepr.showAnsiCode}")

  private def buildFlag(raw: RawParamRepr, ownerName: String)(using Quotes): Expr[ExecutableParser[?]] = {
    val absentValue: Expr[Boolean] =
      defaultTerm(raw, ownerName) match
        case Some(rhs) => rhs.asExprOf[Boolean]
        case None      => '{ false }
    '{ CLIExecutableParser(NamedArgsParser.Flag(${ Expr(longName(raw)) }, ${ shortNameExpr(raw) }, $absentValue, ${ subHelpExpr(raw) })) }
  }

  private def buildToggle(raw: RawParamRepr)(using Quotes): Expr[ExecutableParser[?]] = {
    val longNames: ToggleLongNameRepr =
      ToggleLongNameRepr
        .resolve(
          baseName = raw.annot_longName.fold(raw.valDef.name)(_.name),
          truePrefix = raw.annot_longName_truePrefix.map(_.prefix),
          falsePrefix = raw.annot_longName_falsePrefix.map(_.prefix),
          trueName = raw.annot_longName_trueName.map(_.name),
          falseName = raw.annot_longName_falseName.map(_.name),
        )
        .fold(raw.failAtVal(_), identity)
    val longNamesExpr: Expr[ToggleLongNameRepr] = longNames match
      case ToggleLongNameRepr.PrefixTrue(tp, base)     => '{ ToggleLongNameRepr.PrefixTrue(${ Expr(tp) }, ${ Expr(base) }) }
      case ToggleLongNameRepr.PrefixFalse(fp, base)    => '{ ToggleLongNameRepr.PrefixFalse(${ Expr(fp) }, ${ Expr(base) }) }
      case ToggleLongNameRepr.PrefixBoth(tp, fp, base) => '{ ToggleLongNameRepr.PrefixBoth(${ Expr(tp) }, ${ Expr(fp) }, ${ Expr(base) }) }
      case ToggleLongNameRepr.Explicit(trueN, falseN)  => '{ ToggleLongNameRepr.Explicit(${ Expr(trueN) }, ${ Expr(falseN) }) }
    val shortNamesExpr: Expr[Option[(Char, Char)]] = (raw.annot_shortName_trueName, raw.annot_shortName_falseName) match
      case (Some(t), Some(f)) => '{ Some((${ Expr(t.name) }, ${ Expr(f.name) })) }
      case _                  => '{ None }
    '{ CLIExecutableParser(NamedArgsParser.Toggle($longNamesExpr, $shortNamesExpr, ${ subHelpExpr(raw) })) }
  }

  private def buildCustom(raw: RawParamRepr)(using Quotes): Expr[ExecutableParser[?]] =
    raw.typeRepr.asTypeOf match
      case '[t] =>
        val parser = Implicits.searchRequired[ArgsParser[t]](
          s"Missing ArgsParser for @custom ${raw.typeRepr.showAnsiCode}",
          raw.valPosition,
        )
        '{ CLIExecutableParser($parser) }
      case _ => raw.failAtVal(s"Unable to build @custom param for ${raw.typeRepr.showAnsiCode}")

  // Search PlainText first, then Json. Returns the schema expr typed as `AnySchemaT[t]` (a union member).
  private def anySchemaExpr[t: Type](raw: RawParamRepr)(using Quotes): Expr[oxygen.schema.AnySchemaT[t]] =
    Implicits.searchOption[oxygen.schema.PlainTextSchema[t]] match
      case Some(s) => '{ $s: oxygen.schema.AnySchemaT[t] }
      case None    =>
        Implicits.searchOption[oxygen.schema.JsonSchema[t]] match
          case Some(s) => '{ $s: oxygen.schema.AnySchemaT[t] }
          case None    => raw.failAtVal(s"Missing PlainTextSchema/JsonSchema for ${raw.typeRepr.showAnsiCode}")

  private def buildEnvVar(raw: RawParamRepr, annotName: String, ownerName: String)(using Quotes): Expr[ExecutableParser[?]] = {
    val varName: String = if annotName.nonEmpty then annotName else screamingSnake(raw.valDef.name)
    raw.typeRepr.dealias match
      case AppliedType(constructor, inner :: Nil) if constructor.typeSymbol.fullName == "scala.Option" =>
        inner.asTypeOf match
          case '[t] =>
            val schema = anySchemaExpr[t](raw)
            '{ NonCLIExecutableParser.OptionalEnvVar(NonCLIExecutableParser.SingleEnvVar[t](${ Expr(varName) }, $schema)) }
          case _ => raw.failAtVal(s"Unable to build optional @envVar for ${raw.typeRepr.showAnsiCode}")
      case _ =>
        raw.typeRepr.asTypeOf match
          case '[t] =>
            val schema = anySchemaExpr[t](raw)
            val single = '{ NonCLIExecutableParser.SingleEnvVar[t](${ Expr(varName) }, $schema) }
            defaultTerm(raw, ownerName) match
              case Some(rhs) => '{ NonCLIExecutableParser.DefaultEnvVar($single, ${ rhs.asExprOf[t] }) }
              case None      => single
          case _ => raw.failAtVal(s"Unable to build @envVar for ${raw.typeRepr.showAnsiCode}")
  }

  private def buildEnvConfig(raw: RawParamRepr, envName: String, defaultPath: String, ownerName: String)(using Quotes): Expr[ExecutableParser[?]] = {
    val stringSchema: Expr[oxygen.schema.AnySchemaT[String]] = '{ oxygen.schema.PlainTextSchema.string }
    def pathVar: Expr[NonCLIExecutableParser.EnvVar[String]] =
      if defaultPath.nonEmpty then '{ NonCLIExecutableParser.DefaultEnvVar(NonCLIExecutableParser.SingleEnvVar[String](${ Expr(envName) }, $stringSchema), ${ Expr(defaultPath) }) }
      else '{ NonCLIExecutableParser.SingleEnvVar[String](${ Expr(envName) }, $stringSchema) }
    def jsonDecoder[t: Type]: Expr[oxygen.json.JsonDecoder[t]] =
      Implicits.searchRequired[oxygen.json.JsonDecoder[t]](s"Missing JsonDecoder for @envConfig ${raw.typeRepr.showAnsiCode}", raw.valPosition)

    type T
    given tType: Type[T] = raw.typeRepr.dealias.asTypeOf

    tType match {
      case '[Option[t]] =>
        '{ NonCLIExecutableParser.OptionalEnvConfig(NonCLIExecutableParser.SingleEnvConfig[t]($pathVar, ${ jsonDecoder[t] })) }
      case _ =>
        val single = '{ NonCLIExecutableParser.SingleEnvConfig[T]($pathVar, ${ jsonDecoder[T] }) }
        defaultTerm(raw, ownerName) match
          case Some(rhs) => '{ NonCLIExecutableParser.DefaultEnvConfig($single, ${ rhs.asExprOf[T] }) }
          case None      => single
    }
  }

  /** Combine boxed param parsers into a single parser that yields the values as a `List[Any]`. */
  def combineToList(params: List[Expr[ExecutableParser[Any]]])(using Quotes): Expr[ExecutableParser[List[Any]]] =
    params.foldLeft[Expr[ExecutableParser[List[Any]]]]('{ ExecutableParser.Empty.as(List.empty[Any]) }) { (acc, p) =>
      '{ ($acc ^>>&&## $p).map { case (list, value) => list :+ value } }
    }

  /** Call `instance.method(argExprs...)`, handling zero-arg / zero-param-clause methods. */
  def callMethod(instance: Expr[?], method: oxygen.quoted.Symbol, argExprs: List[Expr[?]])(using Quotes): Expr[?] = {
    val base = instance.toTerm.select(method)
    if argExprs.isEmpty then
      method.tree.narrowOpt[DefDef] match
        case Some(d) if d.paramss.isEmpty                  => base.etaExpand(method.owner).asExpr
        case Some(d) if d.paramss.forall(_.params.isEmpty) => base.appliedToNone.asExpr
        case _                                             => base.appliedToArgs(argExprs.map(_.toTerm)).asExpr
    else base.appliedToArgs(argExprs.map(_.toTerm)).asExpr
  }

  /** Recover typed args from a parsed `List[Any]`. */
  def typedListArgs(listExpr: Expr[List[Any]], typeReprs: List[TypeRepr])(using Quotes): List[Expr[?]] =
    typeReprs.zipWithIndex.map { (tpe, idx) =>
      tpe.asTypeOf match
        case '[t] => '{ $listExpr(${ Expr(idx) }).asInstanceOf[t] }
        case _    => report.errorAndAbort(s"Unable to type arg ${tpe.showAnsiCode}")
    }

}
