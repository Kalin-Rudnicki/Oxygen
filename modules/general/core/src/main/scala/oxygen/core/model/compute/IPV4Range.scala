package oxygen.core.model.compute

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.StringCodec
import oxygen.meta.{given, *}
import oxygen.quoted.*
import scala.Ordering.Implicits.infixOrderingOps
import scala.quoted.*

final case class IPV4Range(min: IPV4, max: IPV4) {

  def contains(ip: IPV4): Boolean = ip >= min && ip <= max

  override def toString: String = s"$min-$max"

}
object IPV4Range {

  inline def apply(inline range: String): IPV4Range = ${ applyImpl('range) }
  private def applyImpl(rangeExpr: Expr[String])(using Quotes): Expr[IPV4Range] = compileTimeEvalOption(rangeExpr)(IPV4Range.parse)

  private val reg = "^([^\\-]+)-([^\\-]+)$".r
  def parse(in: String): Option[IPV4Range] = in match
    case reg(IPV4(min), IPV4(max)) => IPV4Range(min, max).some
    case _                         => None
  def unapply(in: String): Option[IPV4Range] = parse(in)

  given codec: StringCodec[IPV4Range] = StringCodec.string.transformOption(IPV4Range.parse, _.toString)

  given toExpr: ToExprT[IPV4Range] = ToExprT.derived

}
