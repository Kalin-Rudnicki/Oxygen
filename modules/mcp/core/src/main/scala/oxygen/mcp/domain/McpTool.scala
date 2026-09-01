package oxygen.mcp.domain

import oxygen.mcp.api.model as API
import oxygen.mcp.domain.model.*
import zio.*

/**
  * One hosted MCP tool: its advertised metadata ([[Tool]]) plus a handler that runs a `tools/call`.
  *
  * `handle` fails its effect channel with a protocol [[McpError]] — argument-decode failures and a
  * consumer error `E` (mapped through [[McpResponseSchema.Failure]]) surface as an [[McpError]], which
  * the HTTP layer turns into the right status (e.g. an auth error -> `401` + `WWW-Authenticate`). A
  * successful call yields an [[McpToolResult]].
  */
final case class McpTool[Api](
    tool: API.response.Tool,
    requiresAuth: Boolean,
    handle: Api => McpToolInput => ZIO[Scope, McpError, McpToolResult],
) {

  /** Bind the API impl, yielding an api-erased tool the server can host alongside others. */
  def apply(api: Api): AppliedMcpTool =
    AppliedMcpTool(tool, requiresAuth, handle(api))

}
