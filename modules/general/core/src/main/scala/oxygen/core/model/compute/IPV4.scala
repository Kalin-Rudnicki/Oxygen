package oxygen.core.model.compute

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.StringCodec
import oxygen.meta.{*, given}
import oxygen.quoted.*
import scala.quoted.*

final class IPV4 private (val bits: Long) {

  lazy val _1: Int = ((bits >> 24) & 0xff).toInt
  lazy val _2: Int = ((bits >> 16) & 0xff).toInt
  lazy val _3: Int = ((bits >> 8) & 0xff).toInt
  lazy val _4: Int = (bits & 0xff).toInt

  def +(inc: Long): IPV4 = IPV4(bits + inc)
  def -(inc: Long): IPV4 = this + (-inc)
  def +?(inc: Long): IPV4 = this.addAvoidingTrailing_0_255(inc)
  def -?(inc: Long): IPV4 = this.addAvoidingTrailing_0_255(-inc)

  def previous: IPV4 = this - 1
  def next: IPV4 = this + 1

  def networkMask(mask: IPV4CIDR.Prefix): IPV4 = IPV4.fromLong(this.bits & mask.networkMask)
  def broadcastMask(mask: IPV4CIDR.Prefix): IPV4 = IPV4.fromLong(this.bits | mask.broadcastMask)

  // NOTE : [[this]] is not allowed to end with ".0" or ".255", because there is nothing sensible you can do to account for this
  def addAvoidingTrailing_0_255(inc: Long): IPV4 =
    _4 match {
      case 0 | 255      => throw new RuntimeException("IPV4 `this.addAvoidingTrailing_0_255(inc)` requires that `this` does not end in `.0` or `.255`")
      case _ if inc > 0 =>
        val offset4: Long = _4 - 1
        val adjustedBits: Long = bits - offset4
        val adjustedInc: Long = inc + offset4
        val skipAdjustor: Long = adjustedInc / 254 * 2
        IPV4(adjustedBits + adjustedInc + skipAdjustor)
      case _ if inc < 0 =>
        val offset4: Long = 254 - _4
        val adjustedBits: Long = bits + offset4
        val adjustedInc: Long = inc - offset4
        val skipAdjustor: Long = adjustedInc / 254 * 2
        IPV4(adjustedBits + adjustedInc + skipAdjustor)
      case _ =>
        this
    }

  override def toString: String = s"${_1}.${_2}.${_3}.${_4}"

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: IPV4 => this.bits == that.bits
    case _          => false

}
object IPV4 {

  def apply(bits: Long): IPV4 = {
    val masked: Long = bits & 0xffffffff
    if masked == bits then new IPV4(masked)
    else throw new RuntimeException(s"Invalid IPV4 bits : 0x${bits.toHexString}")
  }
  def fromLong(bits: Long): IPV4 = IPV4(bits)
  def apply(_1: Int, _2: Int, _3: Int, _4: Int): IPV4 = new IPV4(segment.safeShifted(_1, 24) | segment.safeShifted(_2, 16) | segment.safeShifted(_3, 8) | segment.safeShifted(_4, 0))

  inline def apply(inline segments: String): IPV4 = ${ applyImpl('segments) }
  private def applyImpl(segmentsExpr: Expr[String])(using Quotes): Expr[IPV4] = compileTimeEvalOption(segmentsExpr)(IPV4.parse)

  private object segment {

    inline def safe(inline segment: Int): Long = {
      val long: Long = segment.toLong
      val masked: Long = long & 0xff
      if masked == long then masked
      else throw new RuntimeException(s"Invalid IPV4 segment : 0x${segment.toHexString}")
    }
    inline def safeShifted(inline segment: Int, inline shift: Int): Long =
      safe(segment) << shift

    def unapply(in: String): Option[Int] = in.toIntOption.filter(i => i >= 0 && i <= 255)

  }
  private val reg = "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$".r

  def parse(in: String): Option[IPV4] = in match
    case reg(segment(a), segment(b), segment(c), segment(d)) => IPV4(a, b, c, d).some
    case _                                                   => None
  def unapply(in: String): Option[IPV4] = parse(in)

  given codec: StringCodec[IPV4] = StringCodec.string.transformOption(IPV4.parse, _.toString)
  given ordering: Ordering[IPV4] = Ordering.by(_.bits)

  private[compute] def of(_1: Int, _2: Int, _3: Int, _4: Int): IPV4 = IPV4(_1, _2, _3, _4)

  given toExpr: ToExprT[IPV4] =
    new ToExprT[IPV4] {
      override def apply(x: IPV4)(using Type[IPV4], Quotes): Expr[IPV4] =
        '{ IPV4(${ Expr(x.bits) }) }
    }

}
