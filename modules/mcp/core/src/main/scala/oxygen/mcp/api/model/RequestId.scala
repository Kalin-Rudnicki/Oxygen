package oxygen.mcp.api.model

import oxygen.json.*
import oxygen.schema.JsonSchema

/**
  * The JSON-RPC request `id` (MCP `2026-07-28`) — the spec allows `string | number`, so it is modeled
  * as arbitrary [[oxygen.json.Json]]. Opaque (`<: Json`) so a correlation id can't be silently swapped
  * for any other free-form json, yet still usable as a [[oxygen.json.Json]] without unwrapping.
  */
opaque type RequestId <: Json = Json
object RequestId {

  def wrap(json: Json): RequestId = json

  given JsonSchema[RequestId] = JsonSchema.json.transform(wrap, x => x)

}
