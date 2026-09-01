package oxygen.mcp.api.model

import oxygen.predef.core.*

/**
  * An MCP protocol revision. Known revisions are [[ProtocolVersion.Known]]; any other string
  * round-trips through [[ProtocolVersion.Unknown]]. Encoded by its `value` (via `toString`).
  */
sealed trait ProtocolVersion derives EnumWithOther {
  val value: String
  override final def toString: String = value
}
object ProtocolVersion {

  def apply(value: String): ProtocolVersion = EnumWithOther[ProtocolVersion].decode(value)

  sealed abstract class Known(final val value: String) extends ProtocolVersion derives StrictEnum

  case object V2025_11_25 extends ProtocolVersion.Known("2025-11-25")
  case object V2026_07_28 extends ProtocolVersion.Known("2026-07-28")

  final case class Unknown(value: String) extends ProtocolVersion

}
