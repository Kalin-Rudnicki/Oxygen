package oxygen.http.schema

import oxygen.json.*

/**
  * The MCP-specific facet of an endpoint, derived from `@mcp.tool` / `@mcp.auth` and carried on
  * [[oxygen.http.server.EndpointSchema]]. Its presence means the endpoint is exposed as an MCP tool;
  * the MCP middleware consumes it directly (no annotation re-parsing) and it is threaded into the
  * compiled API spec so the docs UI can render an MCP badge.
  *
  * @param authParamName the function-parameter name fed by the validated MCP bearer token. It is
  *                      stripped from the tool's input schema and injected (as the request's
  *                      `Authorization` value) when the call is reconstructed.
  *
  * Scope enforcement is not per-tool: required scopes are configured once on the resource server
  * (`McpAuthConfig.requiredScopes`) and apply to every authed tool.
  */
final case class McpEndpointSchema(
    toolName: String,
    description: Option[String],
    inputSchema: Json,
    authParamName: Option[String],
) derives JsonCodec {
  def requiresAuth: Boolean = authParamName.nonEmpty
}
