package oxygen.crypto.model

import oxygen.json.*

/**
  * The token-endpoint response (RFC 6749 §5.1) returned for the `authorization_code` and
  * `refresh_token` grants. `expiresIn` is the access token's lifetime in seconds.
  */
final case class OidcTokenResponse(
    @jsonField("access_token") accessToken: String,
    @jsonField("token_type") tokenType: String,
    @jsonField("expires_in") expiresIn: Option[Long] = None,
    @jsonField("refresh_token") refreshToken: Option[String] = None,
    @jsonField("id_token") idToken: Option[String] = None,
    @jsonField("scope") scope: Option[String] = None,
) derives JsonCodec
