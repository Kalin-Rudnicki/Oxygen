package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.*
import oxygen.json.JsonDecoder
import oxygen.quoted.*
import scala.quoted.*

sealed trait ParamRepr {
  val raw: RawParamRepr

  lazy val docs: List[String] = raw.annot_doc.fold(Nil)(_.parts)

}
object ParamRepr {

  def extract(raw: RawParamRepr, ownerName: String, defaultSyms: Map[(String, Int), Term])(using Quotes): ParamRepr = {
    def doSummon[T: Type]: Expr[T] =
      Implicits.searchOption[T].getOrElse { raw.failAtVal(s"Missing given instance: ${TypeRepr.of[T].showAnsiCode}") }

    def extractFlagAbsentValue: Boolean =
      defaultSyms.get((ownerName, raw.paramIdx)) match
        case None        => false
        case Some(rhs)   =>
          rhs.asExprOf[Boolean].evalOption.getOrElse { raw.failAtVal("Unable to evaluate @flag default") }

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
        val make: Expr[PositionalArgsParser.Builder[raw.T]] = doSummon
        new Positional(raw)(longName, make)
      case named() =>
        val longName: String = raw.annot_longName.fold(raw.valDef.name)(_.name)
        val shortName: Defaultable.Opt[Char] = ??? // FIX-PRE-MERGE (KR) :
        val make: Expr[PositionalArgsParser.Builder[raw.T]] = doSummon
        new Named(raw)(longName, shortName, make)
      case config(env) =>
        val decoder: Expr[JsonDecoder[raw.T]] = doSummon
        new Config(raw)(env, decoder)
      case flag() =>
        val longName: String = raw.annot_longName.fold(raw.valDef.name)(_.name)
        new Flag(raw)(longName, extractFlagAbsentValue)
      case toggle() =>
        new Toggle(raw)(extractToggleLongName)
      case custom() =>
        val make: Expr[ArgsParser[raw.T]] = doSummon
        new Custom(raw)(make)
    }
  }

  final class Positional(
      val raw: RawParamRepr,
  )(
      val longName: String,
      val make: Expr[PositionalArgsParser.Builder[raw.T]],
  ) extends ParamRepr

  final class Named(
      val raw: RawParamRepr,
  )(
      val longName: String,
      val shortName: Defaultable.Opt[Char],
      val make: Expr[PositionalArgsParser.Builder[raw.T]],
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
