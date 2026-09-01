package oxygen.mcp.api.model.response

import oxygen.mcp.api.model.{JsonRpcVersion, RequestId}
import oxygen.schema.JsonSchema

/**
  * The server -> client JSON-RPC failure response envelope (MCP `2026-07-28`). Like [[SuccessResponse]]
  * it carries no `method` discriminator — the client correlates by `id` — and pairs the request `id`
  * with the [[JsonRpcError]] describing the failure.
  */
final case class ErrorResponse(
    jsonrpc: JsonRpcVersion,
    id: RequestId,
    error: JsonRpcError,
) derives JsonSchema
