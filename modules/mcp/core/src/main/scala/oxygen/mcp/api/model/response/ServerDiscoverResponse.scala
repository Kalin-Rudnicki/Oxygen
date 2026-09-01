package oxygen.mcp.api.model.response

import oxygen.mcp.api.model.{Implementation, ProtocolVersion}
import oxygen.schema.JsonSchema

/**
  * The `server/discover` result (MCP `2026-07-28`) — the stateless replacement for `initializeResult`.
  * The request is [[oxygen.mcp.api.model.request.ServerRequest.ServerDiscover]].
  */
final case class ServerDiscoverResponse(
    resultType: ResultType,
    supportedVersions: List[ProtocolVersion],
    capabilities: ServerCapabilities,
    instructions: Option[String],
    serverInfo: Option[Implementation],
    ttlMs: Long,
    cacheScope: CacheScope,
) derives JsonSchema
