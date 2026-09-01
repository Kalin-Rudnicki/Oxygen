package oxygen.mcp.api.model.request

import oxygen.json.*
import oxygen.mcp.api.model.{Implementation, ProtocolVersion}
import oxygen.schema.JsonSchema

/**
  * The `_meta` object carried on **every** MCP `2026-07-28` request — the stateless replacement for
  * the init handshake. `protocolVersion` and `clientCapabilities` are required and declared
  * per-request (servers MUST NOT infer them from prior requests). Wire keys are namespaced under
  * the `io.modelcontextprotocol/` prefix; `progressToken` is un-namespaced.
  */
final case class RequestMeta(
    @jsonField("io.modelcontextprotocol/protocolVersion") protocolVersion: ProtocolVersion,
    @jsonField("io.modelcontextprotocol/clientCapabilities") clientCapabilities: ClientCapabilities,
    @jsonField("io.modelcontextprotocol/clientInfo") clientInfo: Option[Implementation],
    @jsonField("io.modelcontextprotocol/logLevel") logLevel: Option[LoggingLevel], // deprecated (2026-07-28, SEP-2577)
    progressToken: Option[Json], // string | number
) derives JsonSchema
