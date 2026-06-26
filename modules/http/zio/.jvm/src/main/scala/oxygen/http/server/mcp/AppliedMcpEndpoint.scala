package oxygen.http.server.mcp

import oxygen.http.schema.McpEndpointSchema
import oxygen.http.server.McpInput
import zio.*

/** An [[McpEndpoint]] with its API impl already applied — what an (API-erased) [[McpServer]] actually hosts. */
final case class AppliedMcpEndpoint(
    schema: McpEndpointSchema,
    handle: McpInput => URIO[Scope, McpToolResult],
) {
  def requiresAuth: Boolean = schema.requiresAuth
}
