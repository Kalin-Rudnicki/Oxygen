package oxygen.crypto.model

import java.time.Instant
import oxygen.json.*
import oxygen.predef.core.*

/**
  * The standard RFC 7519 registered claims (plus the OAuth `scope` claim), modeled as third parties
  * actually emit them — the loose form, deliberately distinct from [[JWT.StandardPayload]] (which is
  * oxygen's opinionated *issuance* shape: `jti` a UUID, no `iss`/`aud`).
  *
  * This is the *consumption / validation* shape used to validate third-party (e.g. OAuth) access
  * tokens — every claim optional, `jti` a plain `String` (not a UUID), and `aud` decoding from either
  * a single string or an array of strings (see [[Audience]]).
  */
final case class RegisteredClaims(
    @jsonField("iss") issuer: Option[String] = None,
    @jsonField("sub") subject: Option[String] = None,
    @jsonField("aud") audience: Audience = Audience.empty,
    @jsonField("exp") expiresAt: Option[Instant] = None,
    @jsonField("nbf") validAfter: Option[Instant] = None,
    @jsonField("iat") issuedAt: Option[Instant] = None,
    @jsonField("jti") tokenId: Option[String] = None,
    @jsonField("scope") scope: Option[Scopes] = None,
) derives JsonCodec

/** The JWT `aud` claim: either a single string or an array of strings (RFC 7519 §4.1.3). */
final case class Audience(values: List[String]) {
  def contains(audience: String): Boolean = values.contains(audience)
  def isEmpty: Boolean = values.isEmpty
  def nonEmpty: Boolean = values.nonEmpty
}
object Audience {

  val empty: Audience = Audience(Nil)
  def one(audience: String): Audience = Audience(audience :: Nil)

  given JsonCodec[Audience] =
    JsonCodec.json.transformOrFail(
      {
        case Json.Str(s)     => Audience(s :: Nil).asRight
        case Json.Arr(elems) =>
          elems.toList.foldRight[Either[String, List[String]]](Nil.asRight) {
            case (Json.Str(s), Right(acc)) => (s :: acc).asRight
            case (other, Right(_))         => s"`aud` array element is not a string: ${other.showCompact}".asLeft
            case (_, left)                 => left
          }.map(Audience(_))
        case Json.Null => Audience.empty.asRight
        case other     => s"`aud` is not a string or array of strings: ${other.showCompact}".asLeft
      },
      {
        case Audience(s :: Nil) => Json.Str(s)
        case Audience(many)     => Json.Arr(many.map[Json](Json.Str(_)).toArraySeq)
      },
    )

}
