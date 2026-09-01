package oxygen.mcp.api.model.response

import oxygen.predef.core.*

/**
  * The `resultType` every `2026-07-28` result MUST carry (schema: `"complete" | "input_required" |
  * string`; an absent field is treated as `"complete"` for backward compat). We only ever emit
  * `complete` — the MRTR `input_required` flow is modeled but not yet produced.
  */
enum ResultType(final val value: String) derives StrictEnum {
  case Complete extends ResultType("complete")
  case InputRequired extends ResultType("input_required")

  override final def toString: String = value
}
