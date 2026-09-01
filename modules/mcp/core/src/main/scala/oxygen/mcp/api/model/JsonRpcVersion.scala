package oxygen.mcp.api.model

import oxygen.predef.core.*

enum JsonRpcVersion(final val value: String) derives StrictEnum {
  case V2 extends JsonRpcVersion("2.0")

  override final def toString: String = value
}
