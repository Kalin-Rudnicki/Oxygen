package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.*
import oxygen.quoted.*
import scala.quoted.*

sealed trait ParamRepr {
  val raw: RawParamRepr

  lazy val docs: List[String] = raw.annot_doc.fold(Nil)(_.parts)

}
object ParamRepr {

  def extract(raw: RawParamRepr)(using Quotes): ParamRepr = {
    def doSummon[T: Type]: Expr[T] =
      Implicits.searchOption[T].getOrElse { raw.failAtVal(s"Missing given instance: ${TypeRepr.of[T].showAnsiCode}") }

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
      case flag()   => ???
      case toggle() => ???
      case custom() => ???
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

  ///////  ///////////////////////////////////////////////////////////////

}
