package oxygen.oidc

/** Any failure in the OIDC relying-party flow (discovery, callback, token exchange, refresh). */
final case class OidcError(message: String) extends Exception(message)
