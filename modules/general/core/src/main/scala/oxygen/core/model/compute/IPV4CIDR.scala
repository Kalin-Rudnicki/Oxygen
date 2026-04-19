package oxygen.core.model.compute

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.StringCodec
import oxygen.meta.{given, *}
import oxygen.quoted.*
import scala.Ordering.Implicits.infixOrderingOps
import scala.quoted.*

final case class IPV4CIDR(ip: IPV4, prefix: IPV4CIDR.Prefix) {

  lazy val blockStartIP: IPV4 = ip.networkMask(prefix)
  lazy val blockEndIP: IPV4 = ip.broadcastMask(prefix)
  def canonicalRouterIP: IPV4 = blockStartIP + 1

  def blockStartCIDR: IPV4CIDR.BlockStart = IPV4CIDR.BlockStart(IPV4CIDR(blockStartIP, prefix))
  def blockEndCIDR: IPV4CIDR.BlockEnd = IPV4CIDR.BlockEnd(IPV4CIDR(blockStartIP, prefix))

  def +(inc: Long): IPV4CIDR = {
    val cidr2: IPV4CIDR = IPV4CIDR(this.ip + inc, prefix)
    if this.contains(cidr2.ip) then cidr2
    else throw new RuntimeException(s"IPV4CIDR add does not land in the same CIDR block : $this + $inc = $cidr2 (${this.blockStartCIDR} != ${cidr2.blockStartCIDR})")
  }
  def -(inc: Long): IPV4CIDR = this + (-inc)
  def +?(inc: Long): IPV4CIDR = {
    val cidr2: IPV4CIDR = IPV4CIDR(this.ip +? inc, prefix)
    if this.contains(cidr2.ip) then cidr2
    else throw new RuntimeException(s"IPV4CIDR add does not land in the same CIDR block : $this +? $inc = $cidr2 (${this.blockStartCIDR} != ${cidr2.blockStartCIDR})")
  }
  def -?(inc: Long): IPV4CIDR = this +? (-inc)
  def <>(that: IPV4CIDR): Boolean = this.intersects(that)
  def ><(that: IPV4CIDR): Boolean = this.disjoint(that)

  def toRange: IPV4Range = IPV4Range(blockStartIP, blockEndIP)

  def contains(that: IPV4): Boolean =
    if prefix.prefixLength == 0 then true
    else if prefix.prefixLength == 32 then that == this.ip
    else that.networkMask(prefix).bits == blockStartIP.bits
  def contains(that: IPV4CIDR): Boolean =
    this.contains(that.ip) && this.prefix.prefixLength <= that.prefix.prefixLength

  def intersects(that: IPV4CIDR): Boolean = (this.blockStartIP <= that.blockEndIP) && (that.blockStartIP <= this.blockEndIP)
  def disjoint(that: IPV4CIDR): Boolean = !this.intersects(that)

  // both IPV4CIDR have the same network and broadcast IP
  def sameSubnet(that: IPV4CIDR): Boolean =
    this.blockStartIP == that.blockStartIP && this.blockEndIP == that.blockEndIP
  // both IPV4CIDR have the same network IP, but not necessarily same broadcast IP
  def sameNetwork(that: IPV4CIDR): Boolean =
    this.blockStartIP == that.blockStartIP

  override def toString: String = s"$ip$prefix"

}
object IPV4CIDR {

  inline def apply(inline cidr: String): IPV4CIDR = ${ applyImpl('cidr) }
  private def applyImpl(cidrExpr: Expr[String])(using Quotes): Expr[IPV4CIDR] = compileTimeEvalOption(cidrExpr)(IPV4CIDR.parse)

  enum Prefix(final val prefixLength: Int) {

    case _0 extends Prefix(0)
    case _1 extends Prefix(1)
    case _2 extends Prefix(2)
    case _3 extends Prefix(3)
    case _4 extends Prefix(4)
    case _5 extends Prefix(5)
    case _6 extends Prefix(6)
    case _7 extends Prefix(7)
    case _8 extends Prefix(8)
    case _9 extends Prefix(9)
    case _10 extends Prefix(10)
    case _11 extends Prefix(11)
    case _12 extends Prefix(12)
    case _13 extends Prefix(13)
    case _14 extends Prefix(14)
    case _15 extends Prefix(15)
    case _16 extends Prefix(16)
    case _17 extends Prefix(17)
    case _18 extends Prefix(18)
    case _19 extends Prefix(19)
    case _20 extends Prefix(20)
    case _21 extends Prefix(21)
    case _22 extends Prefix(22)
    case _23 extends Prefix(23)
    case _24 extends Prefix(24)
    case _25 extends Prefix(25)
    case _26 extends Prefix(26)
    case _27 extends Prefix(27)
    case _28 extends Prefix(28)
    case _29 extends Prefix(29)
    case _30 extends Prefix(30)
    case _31 extends Prefix(31)
    case _32 extends Prefix(32)

    final val networkMask: Long = 0xffffffffL << (32 - prefixLength)
    final val broadcastMask: Long = (~networkMask) & 0xffffffffL

    final val showStr: String = prefixLength.toString
    final val showSlash: String = "/" + showStr
    override final def toString: String = showSlash

  }
  object Prefix {

    lazy val enumValues: Seq[Prefix] = values.toSeq
    lazy val intMap: Map[Int, Prefix] = enumValues.map { p => (p.prefixLength, p) }.toMap
    lazy val strMap: Map[String, Prefix] = enumValues.map { p => (p.showStr, p) }.toMap
    lazy val slashMap: Map[String, Prefix] = enumValues.map { p => (p.showSlash, p) }.toMap
    lazy val strOrSlashMap: Map[String, Prefix] = strMap ++ slashMap

    def unapply(in: String): Option[Prefix] = strMap.get(in)

    given codec: StringCodec[Prefix] = StringCodec.string.transformOption(slashMap.get, _.showSlash)

    given toExpr: ToExprT[IPV4CIDR.Prefix] =
      new ToExprT[Prefix] {
        override def apply(x: Prefix)(using Type[Prefix], Quotes): Expr[Prefix] = x match
          case Prefix._0  => '{ Prefix._0 }
          case Prefix._1  => '{ Prefix._1 }
          case Prefix._2  => '{ Prefix._2 }
          case Prefix._3  => '{ Prefix._3 }
          case Prefix._4  => '{ Prefix._4 }
          case Prefix._5  => '{ Prefix._5 }
          case Prefix._6  => '{ Prefix._6 }
          case Prefix._7  => '{ Prefix._7 }
          case Prefix._8  => '{ Prefix._8 }
          case Prefix._9  => '{ Prefix._9 }
          case Prefix._10 => '{ Prefix._10 }
          case Prefix._11 => '{ Prefix._11 }
          case Prefix._12 => '{ Prefix._12 }
          case Prefix._13 => '{ Prefix._13 }
          case Prefix._14 => '{ Prefix._14 }
          case Prefix._15 => '{ Prefix._15 }
          case Prefix._16 => '{ Prefix._16 }
          case Prefix._17 => '{ Prefix._17 }
          case Prefix._18 => '{ Prefix._18 }
          case Prefix._19 => '{ Prefix._19 }
          case Prefix._20 => '{ Prefix._20 }
          case Prefix._21 => '{ Prefix._21 }
          case Prefix._22 => '{ Prefix._22 }
          case Prefix._23 => '{ Prefix._23 }
          case Prefix._24 => '{ Prefix._24 }
          case Prefix._25 => '{ Prefix._25 }
          case Prefix._26 => '{ Prefix._26 }
          case Prefix._27 => '{ Prefix._27 }
          case Prefix._28 => '{ Prefix._28 }
          case Prefix._29 => '{ Prefix._29 }
          case Prefix._30 => '{ Prefix._30 }
          case Prefix._31 => '{ Prefix._31 }
          case Prefix._32 => '{ Prefix._32 }
      }

  }

  def _0(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._0)
  def _1(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._1)
  def _2(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._2)
  def _3(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._3)
  def _4(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._4)
  def _5(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._5)
  def _6(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._6)
  def _7(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._7)
  def _8(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._8)
  def _9(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._9)
  def _10(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._10)
  def _11(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._11)
  def _12(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._12)
  def _13(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._13)
  def _14(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._14)
  def _15(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._15)
  def _16(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._16)
  def _17(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._17)
  def _18(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._18)
  def _19(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._19)
  def _20(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._20)
  def _21(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._21)
  def _22(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._22)
  def _23(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._23)
  def _24(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._24)
  def _25(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._25)
  def _26(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._26)
  def _27(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._27)
  def _28(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._28)
  def _29(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._29)
  def _30(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._30)
  def _31(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._31)
  def _32(ip: IPV4): IPV4CIDR = IPV4CIDR(ip, IPV4CIDR.Prefix._32)

  private val reg = "^([^/]+)/(\\d+)$".r
  def parse(in: String): Option[IPV4CIDR] = in match
    case reg(IPV4(ip), IPV4CIDR.Prefix(prefix)) => IPV4CIDR(ip, prefix).some
    case _                                      => None
  def unapply(in: String): Option[IPV4CIDR] = parse(in)

  given codec: StringCodec[IPV4CIDR] = StringCodec.string.transformOption(IPV4CIDR.parse, _.toString)

  opaque type BlockStart <: IPV4CIDR = IPV4CIDR
  object BlockStart {

    inline def apply(inline cidr: String): IPV4CIDR.BlockStart = ${ applyImpl('cidr) }
    private def applyImpl(cidrExpr: Expr[String])(using Quotes): Expr[IPV4CIDR.BlockStart] = compileTimeEvalEither(cidrExpr)(IPV4CIDR.BlockStart.parse)

    private[IPV4CIDR] def apply(cidr: IPV4CIDR): IPV4CIDR.BlockStart = cidr

    def parse(in: String): Either[String, IPV4CIDR.BlockStart] =
      IPV4CIDR.codec.decoder.decodeDetailed(in).flatMap(wrap)

    def wrap(cidr: IPV4CIDR): Either[String, IPV4CIDR.BlockStart] =
      Either.cond(cidr.ip == cidr.blockStartIP, cidr, s"IPV4CIDR IP does not reference the start of its block (cidr=$cidr, start=${cidr.blockStartIP})")

    given codec: StringCodec[IPV4CIDR.BlockStart] = IPV4CIDR.codec.transformOrFail(wrap, identity)

    given toExpr: ToExprT[IPV4CIDR.BlockStart] =
      new ToExprT[IPV4CIDR.BlockStart] {
        override def apply(x: IPV4CIDR.BlockStart)(using Type[IPV4CIDR.BlockStart], Quotes): Expr[IPV4CIDR.BlockStart] =
          '{ IPV4CIDR.BlockStart(${ IPV4CIDR.toExpr(x) }) }
      }

  }

  opaque type BlockEnd <: IPV4CIDR = IPV4CIDR
  object BlockEnd {

    inline def apply(inline cidr: String): IPV4CIDR.BlockEnd = ${ applyImpl('cidr) }
    private def applyImpl(cidrExpr: Expr[String])(using Quotes): Expr[IPV4CIDR.BlockEnd] = compileTimeEvalEither(cidrExpr)(IPV4CIDR.BlockEnd.parse)

    private[IPV4CIDR] def apply(cidr: IPV4CIDR): IPV4CIDR.BlockEnd = cidr

    def parse(in: String): Either[String, IPV4CIDR.BlockEnd] =
      IPV4CIDR.codec.decoder.decodeDetailed(in).flatMap(wrap)

    def wrap(cidr: IPV4CIDR): Either[String, IPV4CIDR.BlockEnd] =
      Either.cond(cidr.ip == cidr.blockEndIP, cidr, s"IPV4CIDR IP does not reference the end of its block (cidr=$cidr, end=${cidr.blockEndIP})")

    given codec: StringCodec[IPV4CIDR.BlockEnd] = IPV4CIDR.codec.transformOrFail(wrap, identity)

    given toExpr: ToExprT[IPV4CIDR.BlockEnd] =
      new ToExprT[IPV4CIDR.BlockEnd] {
        override def apply(x: IPV4CIDR.BlockEnd)(using Type[IPV4CIDR.BlockEnd], Quotes): Expr[IPV4CIDR.BlockEnd] =
          '{ IPV4CIDR.BlockEnd(${ IPV4CIDR.toExpr(x) }) }
      }

  }

  given toExpr: ToExprT[IPV4CIDR] =
    new ToExprT[IPV4CIDR] {
      override def apply(x: IPV4CIDR)(using Type[IPV4CIDR], Quotes): Expr[IPV4CIDR] =
        '{ IPV4CIDR(${ Expr(x.ip) }, ${ Expr(x.prefix) }) }
    }

}
