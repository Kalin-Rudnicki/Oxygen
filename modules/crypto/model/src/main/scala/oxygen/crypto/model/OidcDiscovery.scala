package oxygen.crypto.model

import oxygen.json.*

/**
  * The subset of an OpenID Connect discovery document (`/.well-known/openid-configuration`, OIDC
  * Discovery / RFC 8414) that a relying party actually needs. The decoder is non-strict, so the many
  * other advertised fields are simply ignored.
  */
final case class OidcDiscovery(
    @jsonField("issuer") issuer: String,
    @jsonField("authorization_endpoint") authorizationEndpoint: String,
    @jsonField("token_endpoint") tokenEndpoint: String,
    @jsonField("jwks_uri") jwksUri: String,
    @jsonField("userinfo_endpoint") userinfoEndpoint: Option[String] = None,
    @jsonField("end_session_endpoint") endSessionEndpoint: Option[String] = None,
    @jsonField("introspection_endpoint") introspectionEndpoint: Option[String] = None,
) derives JsonCodec
