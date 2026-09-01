package oxygen.mcp.domain

import oxygen.mcp.api.model as API
import oxygen.mcp.domain.model.*
import zio.*

/** An [[McpTool]] with its API impl already applied — what the server actually hosts. */
final case class AppliedMcpTool(
    tool: API.response.Tool,
    requiresAuth: Boolean,
    handle: McpToolInput => ZIO[Scope, McpError, McpToolResult],
)
