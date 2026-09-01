package oxygen.mcp.http

import oxygen.json.*
import oxygen.predef.core.*

/**
  * RFC 9728 Protected Resource Metadata — what the server publishes at
  * `/.well-known/oauth-protected-resource` (and, per the spec's discovery redundancy, at the
  * path-suffixed variant) so a client can find the Authorization Server(s) to get a token from.
  *
  * The server only *names* the AS(es) here; it never fronts login/issuance/registration. Claude reads
  * `authorization_servers[0]` only, so list the primary first.
  */
final case class ProtectedResourceMetadata(
    resource: String, // this server's canonical resource URI (the RFC 8707 `aud` tokens must carry)
    authorizationServers: List[String], // ≥1 AS issuer URL, primary first
    scopesSupported: Set[String],
    bearerMethodsSupported: List[String],
    resourceName: Option[String],
) {

  def toJson: Json = {
    val fields: List[(String, Json)] =
      List[(String, Json)]("resource" -> Json.string(resource)) :::
        List("authorization_servers" -> Json.Arr(authorizationServers.map[Json](Json.string).toArraySeq)) :::
        (if scopesSupported.nonEmpty then List("scopes_supported" -> Json.Arr(scopesSupported.toList.sorted.map[Json](Json.string).toArraySeq)) else Nil) :::
        (if bearerMethodsSupported.nonEmpty then List("bearer_methods_supported" -> Json.Arr(bearerMethodsSupported.map[Json](Json.string).toArraySeq)) else Nil) :::
        resourceName.map(n => "resource_name" -> Json.string(n)).toList
    Json.Obj(fields.toArraySeq)
  }

}
object ProtectedResourceMetadata {

  /** The common case: a resource + its authorization server(s), header bearer method, no scopes/name. */
  def apply(resource: String, authorizationServers: List[String], scopesSupported: Set[String]): ProtectedResourceMetadata =
    ProtectedResourceMetadata(resource, authorizationServers, scopesSupported, List("header"), None)

}
