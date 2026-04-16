package oxygen.core.model.compute

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.StringCodec

final case class IPV4Range(min: IPV4, max: IPV4) {
  override def toString: String = s"$min-$max"
}
object IPV4Range {

  private val reg = "^([^\\-]+)-([^\\-]+)$".r

  def parse(in: String): Option[IPV4Range] = in match
    case reg(IPV4(min), IPV4(max)) => IPV4Range(min, max).some
    case _                         => None
  def unapply(in: String): Option[IPV4Range] = parse(in)

  given codec: StringCodec[IPV4Range] = StringCodec.string.transformOption(IPV4Range.parse, _.toString)

}
