package oxygen.mcp.api.model.response

import oxygen.json.*
import oxygen.mcp.api.model.{JsonRpcVersion, RequestId}
import oxygen.schema.JsonSchema

/**
  * The server -> client JSON-RPC success response envelope (MCP `2026-07-28`). JSON-RPC responses carry
  * no `method` discriminator — the client correlates by `id` — so success and failure are two distinct
  * product shapes rather than a tagged union. The method-specific result payload
  * ([[ServerDiscoverResponse]] / [[ListToolsResponse]] / [[CallToolResponse]]) is carried pre-encoded
  * as [[oxygen.json.Json]] in [[SuccessResponse.result]].
  */
final case class SuccessResponse(
    jsonrpc: JsonRpcVersion,
    id: RequestId,
    result: Json,
) derives JsonSchema
