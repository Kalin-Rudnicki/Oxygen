package oxygen.crypto.model

import oxygen.json.*
import oxygen.predef.core.*

/**
  * The JOSE header of a JWT. Per RFC 7515 §4.1 only `alg` is REQUIRED; `typ` (RFC 7519 §5.1) and `kid`
  * (RFC 7515 §4.1.4) are OPTIONAL. They are modeled as `Option` so that tokens from third-party
  * issuers that omit `typ` (e.g. Dex) still decode, and so `kid` is available for JWKS key selection.
  * On encode, `None` fields are omitted; oxygen-issued tokens continue to carry `typ = "JWT"`.
  */
final case class JWTHeader(
    alg: JWTHeader.Alg,
    typ: Option[JWTHeader.Type] = None,
    kid: Option[String] = None,
) derives JsonCodec
object JWTHeader {

  enum Alg derives StrictEnum {
    case HS256, RS256, ES256, ES384, ES512, none

    final def toJWTHeader: JWTHeader = JWTHeader(this, Some(Type.JWT))

  }

  enum Type derives StrictEnum { case JWT }

}
