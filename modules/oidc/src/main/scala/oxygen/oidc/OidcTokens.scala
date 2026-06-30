package oxygen.oidc

import java.time.Instant

/**
  * The tokens held (in memory only) for an authenticated session. `expiresAt` is the access token's
  * computed expiry (issuance time + `expires_in`); `None` means the IdP gave no lifetime, so we treat
  * it as non-expiring locally and lean on a 401 to drive refresh.
  */
final case class OidcTokens(
    accessToken: String,
    refreshToken: Option[String],
    idToken: Option[String],
    expiresAt: Option[Instant],
)
