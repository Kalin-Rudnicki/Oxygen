package oxygen.http.server.mcp

/** The payload of a `tools/call` result: a JSON value (rendered as a text content block) + the error flag. */
final case class McpToolResult(results: List[McpResponseContent], isError: Boolean)
object McpToolResult {

  def decodeError(message: String): McpToolResult =
    McpToolResult(
      McpResponseContent.plainText(s"invalid arguments: $message") :: Nil,
      isError = true,
    )

}
