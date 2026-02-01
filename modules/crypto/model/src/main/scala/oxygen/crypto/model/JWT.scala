package oxygen.crypto.model

import java.time.Instant
import java.util.UUID
import oxygen.json.*
import oxygen.json.syntax.json.*
import oxygen.predef.core.*

final case class JWT[A](
    payload: A,
    token: BearerToken,
)
object JWT {

  type Std[P] = JWT[StandardPayload[P]]

  final case class StandardPayload[P](
      @jsonField("jti") tokenId: UUID,
      @jsonField("iat") issuedAt: Instant,
      @jsonField("nbf") validAfter: Option[Instant],
      @jsonField("exp") expiresAt: Option[Instant],
      @jsonFlatten payload: P,
  ) derives JsonCodec

  def decode[A: JsonDecoder](bearerToken: BearerToken): Either[String, JWT[A]] =
    bearerToken.payload.fromJsonString[A].leftMap(_.getMessage).map(JWT(_, bearerToken))

  /**
    * Accepts format(s):
    * - "Bearer headerBase64.payloadBase64.signatureBase64"
    */
  def decodeBearer[A: JsonDecoder](string: String): Either[String, JWT[A]] =
    BearerToken.decodeBearer(string).flatMap(decode[A])

  /**
    * Accepts format(s):
    * - "headerBase64.payloadBase64.signatureBase64"
    */
  def decodeToken[A: JsonDecoder](string: String): Either[String, JWT[A]] =
    BearerToken.decodeToken(string).flatMap(decode[A])

  /**
    * Accepts format(s):
    * - "Bearer headerBase64.payloadBase64.signatureBase64"
    * - "headerBase64.payloadBase64.signatureBase64"
    */
  def decodeBearerOrToken[A: JsonDecoder](string: String): Either[String, JWT[A]] =
    BearerToken.decodeBearerOrToken(string).flatMap(decode[A])

}
