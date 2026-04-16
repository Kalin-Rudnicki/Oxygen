package oxygen.core.model.compute

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.StringCodec

final case class IPV4 private (_1: Int, _2: Int, _3: Int, _4: Int) {

  val bits: Long =
    (_1.toLong << 24) |
      (_2.toLong << 16) |
      (_3.toLong << 8) |
      (_4.toLong << 0)

  def +(inc: Int): IPV4 = {
    val newBits: Long = bits + inc
    if newBits <= 0 || newBits > 0xffffffffL then
      throw new RuntimeException(s"IPV4 overflow : $this + $inc = $newBits")

    IPV4.fromLong(newBits)
  }
  def -(inc: Int): IPV4 = this + (-inc)
  def previous: IPV4 = this - 1
  def next: IPV4 = this + 1

  def masked(mask: IPV4CIDR.Prefix): IPV4 = IPV4.fromLong(this.bits & mask.mask)

  override def toString: String = s"${_1}.${_2}.${_3}.${_4}"

}
object IPV4 {

  private object segment {
    def unapply(in: String): Option[Int] = in.toIntOption.filter(i => i >= 0 && i <= 255)
  }
  private val reg = "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$".r

  def parse(in: String): Option[IPV4] = in match
    case reg(segment(a), segment(b), segment(c), segment(d)) => IPV4(a, b, c, d).some
    case _                                                   => None
  def unapply(in: String): Option[IPV4] = parse(in)

  def fromLong(bits: Long): IPV4 = {
    val a = (bits >> 24) & 0xff
    val b = (bits >> 16) & 0xff
    val c = (bits >> 8) & 0xff
    val d = bits & 0xff
    IPV4(a.toInt, b.toInt, c.toInt, d.toInt)
  }

  given codec: StringCodec[IPV4] = StringCodec.string.transformOption(IPV4.parse, _.toString)
  given ordering: Ordering[IPV4] = Ordering.by(_.bits)

}
