package oxygen.http.server.mcp

import oxygen.http.schema.McpEndpointSchema
import oxygen.http.server.McpInput
import zio.*

/**
  * One derived MCP tool: its advertised metadata (`name` / `description` / `inputSchema`) plus a
  * direct invocation that decodes the JSON `arguments`, runs the underlying handler, and encodes the
  * result — no synthetic HTTP request involved.
  *
  * `invoke` never fails the effect channel: argument-validation and typed handler errors come back as
  * a [[McpToolResult]] with `isError = true` (MCP returns input/execution errors as tool results so
  * the model can self-correct, per spec SEP-1303).
  */
final case class McpEndpoint[-Api](
    schema: McpEndpointSchema,
    handle: Api => McpInput => URIO[Scope, McpToolResult],
) {

  /** Bind the API impl, yielding an api-erased tool the [[McpServer]] can host alongside others. */
  def apply(api: Api): AppliedMcpEndpoint =
    AppliedMcpEndpoint(schema, handle(api))

}
