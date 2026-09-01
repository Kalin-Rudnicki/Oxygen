package oxygen.mcp.api.model.response

import oxygen.schema.JsonSchema

/** The `tools/call` result (MCP `2026-07-28`) — the request is [[oxygen.mcp.api.model.request.ServerRequest.ToolsCall]]. */
final case class CallToolResponse(
    resultType: ResultType,
    content: List[ContentBlock],
    structuredContent: Option[StructuredContent],
    isError: Option[Boolean],
) derives JsonSchema
