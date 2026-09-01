package oxygen.mcp.api.model.response

import oxygen.json.*
import oxygen.schema.JsonSchema

/**
  * The optional `structuredContent` of a `tools/call` result (MCP `2026-07-28`) — a tool's machine-
  * readable payload, mirrored alongside the human-readable `content`. Free-form [[oxygen.json.Json]],
  * opaque (`<: Json`) so it can't be confused with any other free-form json, yet still usable as a
  * [[oxygen.json.Json]] without unwrapping.
  */
opaque type StructuredContent <: Json = Json
object StructuredContent {

  def wrap(json: Json): StructuredContent = json

  given JsonSchema[StructuredContent] = JsonSchema.json.transform(wrap, x => x)

}
