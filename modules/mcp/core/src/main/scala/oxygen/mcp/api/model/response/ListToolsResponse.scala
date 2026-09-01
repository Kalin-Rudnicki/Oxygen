package oxygen.mcp.api.model.response

import oxygen.schema.JsonSchema

/** The `tools/list` result (MCP `2026-07-28`) — the request is [[oxygen.mcp.api.model.request.ServerRequest.ToolsList]]. */
final case class ListToolsResponse(
    resultType: ResultType,
    tools: List[Tool],
    nextCursor: Option[String],
    ttlMs: Long,
    cacheScope: CacheScope,
) derives JsonSchema
