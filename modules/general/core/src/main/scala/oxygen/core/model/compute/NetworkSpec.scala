package oxygen.core.model.compute

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.StringCodec
import oxygen.meta.{given, *}
import oxygen.quoted.*
import scala.quoted.*

sealed trait NetworkSpec {

  def range: IPV4Range
  def show: String

  override final def toString: String = show

}
object NetworkSpec {

  inline def apply(inline spec: String): NetworkSpec = ${ applyImpl('spec) }
  private def applyImpl(specExpr: Expr[String])(using Quotes): Expr[NetworkSpec] = compileTimeEvalOption(specExpr)(NetworkSpec.parse)

  sealed trait Multiple extends NetworkSpec
  object Multiple {

    inline def apply(inline spec: String): NetworkSpec.Multiple = ${ applyImpl('spec) }
    private def applyImpl(specExpr: Expr[String])(using Quotes): Expr[NetworkSpec.Multiple] = compileTimeEvalOption(specExpr)(NetworkSpec.Multiple.parse)

    def parse(in: String): Option[NetworkSpec.Multiple] = in match
      case IPV4Range(out) => NetworkSpec.Range(out).some
      case IPV4CIDR(out)  => NetworkSpec.CIDR(out).some
      case _              => None
    def unapply(in: String): Option[NetworkSpec.Multiple] = parse(in)

    given codec: StringCodec[NetworkSpec.Multiple] = StringCodec.string.transformOption(NetworkSpec.Multiple.parse, _.toString)

    given toExpr: ToExprT[NetworkSpec.Multiple] = ToExprT.derived

  }

  final case class Single(ip: IPV4) extends NetworkSpec {
    override def range: IPV4Range = IPV4Range(ip, ip)
    override def show: String = ip.toString
  }

  final case class Range(range: IPV4Range) extends NetworkSpec.Multiple {
    override def show: String = range.toString
  }

  final case class CIDR(cidr: IPV4CIDR) extends NetworkSpec.Multiple {
    override def range: IPV4Range = cidr.toRange
    override def show: String = cidr.toString
  }

  def parse(in: String): Option[NetworkSpec] = in match
    case IPV4Range(out) => NetworkSpec.Range(out).some
    case IPV4CIDR(out)  => NetworkSpec.CIDR(out).some
    case IPV4(out)      => NetworkSpec.Single(out).some
    case _              => None
  def unapply(in: String): Option[NetworkSpec] = parse(in)

  given codec: StringCodec[NetworkSpec] = StringCodec.string.transformOption(NetworkSpec.parse, _.toString)

  given toExpr: ToExprT[NetworkSpec] = ToExprT.derived

}
