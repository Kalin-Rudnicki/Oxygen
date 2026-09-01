package oxygen.mcp.http

import oxygen.mcp.domain.model.McpPrincipal
import zio.*

/**
  * The one mandatory piece to make an MCP server *authenticated*: turn a presented bearer token into a
  * validated [[McpPrincipal]] (or a failure message). Everything else about OAuth — login, consent,
  * token issuance, client registration — belongs to the external Authorization Server; this is the pure
  * Resource-Server duty (validate + audience-bind).
  *
  * This is a bare seam: `oxygen-mcp-http` bundles no concrete implementation. Supplying no verifier at
  * all (`McpHttp.Auth` with `verifier = None`) is the zero-config default — an unauthenticated server,
  * no `401`, no scope checks.
  */
trait McpTokenVerifier {

  /**
    * Validate the raw bearer (already stripped of the `Bearer ` prefix upstream, guaranteed non-empty).
    * The `String` error is the human-readable reason the HTTP layer surfaces in the `401` body.
    */
  def verify(bearer: String): IO[String, McpPrincipal]

}
