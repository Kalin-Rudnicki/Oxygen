package oxygen.core.model.compute

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.StringCodec

final case class IPV4CIDR private (ip: IPV4, prefix: IPV4CIDR.Prefix) {

  val maskedIp: IPV4 = ip.masked(prefix)

  def contains(test: IPV4): Boolean =
    if prefix.prefixLength == 0 then true
    else if prefix.prefixLength == 32 then test == this.ip
    else test.masked(prefix).bits == maskedIp.bits

  override def toString: String = s"$ip/$prefix"

}
object IPV4CIDR {

  final case class Prefix private (prefixLength: Int) {
    val mask: Long = 0xffffffffL << (32 - prefixLength)
    override def toString: String = prefixLength.toString
  }
  object Prefix {

    def parse(in: String): Option[Prefix] =
      in.toIntOption.filter(i => i >= 0 && i <= 32).map(Prefix(_))
    def unapply(in: String): Option[Prefix] = parse(in)

  }

  private val reg = "^([^/]+)/(\\d+)$".r
  def parse(in: String): Option[IPV4CIDR] = in match
    case reg(IPV4(ip), IPV4CIDR.Prefix(prefix)) => IPV4CIDR(ip, prefix).some
    case _                                      => None
  def unapply(in: String): Option[IPV4CIDR] = parse(in)

  given codec: StringCodec[IPV4CIDR] = StringCodec.string.transformOption(IPV4CIDR.parse, _.toString)

}
