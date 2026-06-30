package oxygen.oidc

/**
  * Configuration for a browser (public, PKCE) OpenID Connect relying party.
  *
  * @param authority             issuer base URL; discovery is fetched from `$authority/.well-known/openid-configuration`
  * @param clientId              the public client id registered with the IdP (no secret — a SPA can't hide one)
  * @param redirectUri           where the IdP sends the user back with the authorization `code`
  * @param scopes                requested scopes; `openid` is required for OIDC
  * @param postLogoutRedirectUri where the IdP sends the user back after end-session, if used
  */
final case class OidcClientConfig(
    authority: String,
    clientId: String,
    redirectUri: String,
    scopes: List[String] = List("openid", "profile", "email"),
    postLogoutRedirectUri: Option[String] = None,
) {
  def discoveryUrl: String = s"${authority.stripSuffix("/")}/.well-known/openid-configuration"
}
