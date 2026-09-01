package oxygen.mcp.domain.model

import oxygen.json.Json

/**
  * A resolved, authenticated caller — what the HTTP layer produces after validating a bearer token and
  * hands to the dispatcher (and on into a tool handler via [[McpToolInput]]).
  *
  * This is pure data: **no** token validation, introspection, or OAuth logic lives in `mcp-core`. All
  * of that ("the complex OAuth stuff") is abstracted behind traits consumed by the `McpHttp` transport
  * in `mcp-http`, which is what constructs an [[McpPrincipal]].
  */
final case class McpPrincipal(
    subject: String,
    scopes: Set[String],
    token: String, // the raw bearer, for handlers that need to call downstream on the user's behalf
    claims: Json, // the full validated claim set, for handler-specific needs
)
