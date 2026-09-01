package oxygen.mcp.domain.model

import oxygen.mcp.api.model as API

/**
  * The outcome of running a tool (domain) — mapped to the wire `CallTool.Result` at the boundary. Per
  * MCP, a tool-execution error is a *result* with `isError = true` (not a JSON-RPC protocol error), so
  * the model can see it and self-correct.
  */
final case class McpToolResult(
    content: List[API.response.ContentBlock],
    structuredContent: Option[API.response.StructuredContent],
    isError: Boolean,
)
object McpToolResult {

  def text(value: String): McpToolResult =
    McpToolResult(API.response.ContentBlock.Text(value, None) :: Nil, None, isError = false)

  def error(message: String): McpToolResult =
    McpToolResult(API.response.ContentBlock.Text(message, None) :: Nil, None, isError = true)

}
