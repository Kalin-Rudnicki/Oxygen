package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.*
import oxygen.json.JsonDecoder
import oxygen.quoted.*
import oxygen.schema.PlainTextSchema
import oxygen.schema.PlainTextSchema.given
import scala.quoted.*

private val __ensureCliSchemaGivens: (PlainTextSchema[String], PlainTextSchema[Int], PlainTextSchema[Double]) =
  (summon[PlainTextSchema[String]], summon[PlainTextSchema[Int]], summon[PlainTextSchema[Double]])

sealed trait ParamRepr {
  val raw: RawParamRepr

  lazy val docs: List[String] = raw.annot_doc.fold(Nil)(_.parts)

}
object ParamRepr {

  def extract(raw: RawParamRepr, ownerName: String, defaultSyms: Map[(String, Int), Term])(using Quotes): ParamRepr = {
    def doSummon[T: Type]: Expr[T] =
      Implicits.searchOption[T].getOrElse { raw.failAtVal(s"Missing given instance: ${TypeRepr.of[T].showAnsiCode}") }

    def extractToggleLongName: ToggleLongNameRepr =
      ToggleLongNameRepr
        .resolve(
          baseName = raw.annot_longName.fold(raw.valDef.name)(_.name),
          truePrefix = raw.annot_longName_truePrefix.map(_.prefix),
          falsePrefix = raw.annot_longName_falsePrefix.map(_.prefix),
          trueName = raw.annot_longName_trueName.map(_.name),
          falseName = raw.annot_longName_falseName.map(_.name),
        )
        .fold(raw.failAtVal(_), identity)

    raw.annot_paramType match {
      case positional() =>
        val longName: String = raw.annot_longName.fold(raw.valDef.name)(_.name)
        new Positional(raw)(longName)
      case named() =>
        val longName: String = raw.annot_longName.fold(raw.valDef.name)(_.name)
        val resolvedShortName: Defaultable.Opt[Char] = raw.annot_shortName match
          case None                               => Defaultable.Default
          case Some(_: oxygen.cli.shortName.none) => Defaultable.Explicit(None)
          case Some(_: oxygen.cli.shortName.auto) => Defaultable.Default
          case Some(s: oxygen.cli.shortName)      => Defaultable.Explicit(Some(s.name))
        new Named(raw)(longName, resolvedShortName)
      case config(env) =>
        type ParamT = raw.T
        given Type[ParamT] = raw.typeRepr.asTypeOf
        val decoder: Expr[JsonDecoder[ParamT]] = doSummon[JsonDecoder[ParamT]]
        new Config(raw)(env, decoder)
      case flag() =>
        val longName: String = raw.annot_longName.fold(raw.valDef.name)(_.name)
        val absentValue: Boolean = defaultSyms.get((ownerName, raw.paramIdx)) match
          case None      => false
          case Some(rhs) => rhs.asExprOf[Boolean].evalOption.getOrElse { raw.failAtVal("Unable to evaluate @flag default") }
        new Flag(raw)(longName, absentValue)
      case toggle() =>
        new Toggle(raw)(extractToggleLongName)
      case custom() =>
        type ParamT = raw.T
        given Type[ParamT] = raw.typeRepr.asTypeOf
        val make: Expr[ArgsParser[ParamT]] = doSummon[ArgsParser[ParamT]]
        new Custom(raw)(make)
    }
  }

  final class Positional(
      val raw: RawParamRepr,
  )(
      val longName: String,
  ) extends ParamRepr

  final class Named(
      val raw: RawParamRepr,
  )(
      val longName: String,
      val shortName: Defaultable.Opt[Char],
  ) extends ParamRepr

  final class Custom(
      val raw: RawParamRepr,
  )(
      val make: Expr[ArgsParser[raw.T]],
  ) extends ParamRepr

  final class Config(
      val raw: RawParamRepr,
  )(
      val envVar: String,
      val decoder: Expr[JsonDecoder[raw.T]],
  ) extends ParamRepr

  final class Toggle(
      val raw: RawParamRepr,
  )(
      val longNames: ToggleLongNameRepr,
  ) extends ParamRepr

  final class Flag(
      val raw: RawParamRepr,
  )(
      val longName: String,
      val absentValue: Boolean,
  ) extends ParamRepr

  ///////  ///////////////////////////////////////////////////////////////

}
