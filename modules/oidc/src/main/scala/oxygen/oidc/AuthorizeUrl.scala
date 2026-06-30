package oxygen.oidc

import oxygen.crypto.model.{OidcDiscovery, Pkce}
import scala.scalajs.js.URIUtils.encodeURIComponent

/** Builds the `/authorize` redirect URL for the Authorization Code + PKCE flow (RFC 6749 §4.1, RFC 7636). */
object AuthorizeUrl {

  /**
    * @param state  CSRF/round-trip binding — opaque, verified on the callback
    * @param nonce  replay binding echoed into the id_token (OIDC Core §3.1.2.1)
    * @param prompt e.g. `Some("none")` for a silent re-auth against an existing IdP session
    */
  def build(
      discovery: OidcDiscovery,
      config: OidcClientConfig,
      pkce: Pkce,
      state: String,
      nonce: String,
      prompt: Option[String] = None,
  ): String = {
    val params: List[(String, String)] =
      List(
        "response_type" -> "code",
        "client_id" -> config.clientId,
        "redirect_uri" -> config.redirectUri,
        "scope" -> config.scopes.mkString(" "),
        "state" -> state,
        "nonce" -> nonce,
        "code_challenge" -> pkce.codeChallenge,
        "code_challenge_method" -> pkce.codeChallengeMethod,
      ) ::: prompt.map(p => "prompt" -> p).toList
    val query = params.map((k, v) => s"${encodeURIComponent(k)}=${encodeURIComponent(v)}").mkString("&")
    s"${discovery.authorizationEndpoint}?$query"
  }

}
