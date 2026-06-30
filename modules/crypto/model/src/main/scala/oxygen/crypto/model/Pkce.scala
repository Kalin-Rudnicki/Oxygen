package oxygen.crypto.model

import oxygen.core.PlatformCompat

/**
  * RFC 7636 Proof Key for Code Exchange material for the OAuth Authorization Code flow used by public
  * (e.g. browser/SPA) clients: a high-entropy `codeVerifier` and its derived `codeChallenge`.
  *
  * The client sends `codeChallenge` (+ method) on the `/authorize` request and the raw `codeVerifier`
  * on the `/token` exchange; the IdP recomputes and compares. This binds the authorization code to the
  * client instance that started the flow, so an intercepted code is useless without the verifier.
  */
final case class Pkce(
    codeVerifier: String,
    codeChallenge: String,
    codeChallengeMethod: String,
)
object Pkce {

  /** Only `S256` is supported; `plain` is deliberately omitted (RFC 7636 §4.2 says use S256 when able). */
  val method: String = "S256"

  /** The RFC 7636 `S256` challenge: base64url-unpadded( SHA-256( ASCII(verifier) ) ). Deterministic. */
  def challengeFor(codeVerifier: String): String =
    Base64.urlEncoder.encodeToString(Sha256.hash(codeVerifier.iterator.map(_.toByte).toArray))

  /** Generate a fresh verifier (32 secure-random bytes, base64url ~43 chars) and its `S256` challenge. */
  def generate: Pkce = {
    val codeVerifier = Base64.urlEncoder.encodeToString(PlatformCompat.secureRandomBytes(32))
    Pkce(codeVerifier, challengeFor(codeVerifier), method)
  }

}
