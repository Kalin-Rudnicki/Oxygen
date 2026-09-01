package oxygen.mcp.api.model.response

import oxygen.json.*

/**
  * The encoded body of a server response (MCP `2026-07-28`) — the method-specific result payload
  * ([[ServerDiscoverResponse]] / [[ListToolsResponse]] / [[CallToolResponse]]) after it has been
  * encoded at the dispatcher boundary. Opaque (`<: Json`) so a response body can't be silently
  * swapped for any other free-form json, yet still usable as a [[oxygen.json.Json]] without
  * unwrapping (it flows straight into [[SuccessResponse.result]]).
  */
opaque type ServerResponse <: Json = Json
object ServerResponse {

  def wrap(json: Json): ServerResponse = json

}
