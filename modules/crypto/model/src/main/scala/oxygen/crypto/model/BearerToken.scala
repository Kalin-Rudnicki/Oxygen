package oxygen.crypto.model

import oxygen.predef.core.*
import oxygen.predef.json.*
import scala.util.Try

final case class BearerToken private[crypto] (
    headerBase64: String,
    header: JWTHeader,
    payloadBase64: String,
    payload: String,
    signatureBase64: String,
) {

  def `headerBase64.payloadBase64`: String = s"$headerBase64.$payloadBase64"

  def token: String = s"$headerBase64.$payloadBase64.$signatureBase64"
  def bearer: String = s"Bearer $token"

}
object BearerToken {

  private[crypto] val bearerReg = "^Bearer ([^. ]*)\\.([^. ]*)\\.([^. ]*)$".r
  private[crypto] val tokenReg = "^([^. ]*)\\.([^. ]*)\\.([^. ]*)$".r

  private def base64Decode(string: String): Either[String, String] =
    Try { new String(Base64.urlDecoder.decode(string)) }.toEither.leftMap(_.safeGetMessage)

  private def build(headerBase64: String, payloadBase64: String, signatureBase64: String): Either[String, BearerToken] =
    for {
      un64edHeader <- base64Decode(headerBase64)
      header <- un64edHeader.fromJsonString[JWTHeader].leftMap(_.getMessage)
      un64edPayload <- base64Decode(payloadBase64)
    } yield BearerToken(
      headerBase64 = headerBase64,
      header = header,
      payloadBase64 = payloadBase64,
      payload = un64edPayload,
      signatureBase64 = signatureBase64,
    )

  /**
    * Accepts format(s):
    * - "Bearer headerBase64.payloadBase64.signatureBase64"
    */
  def decodeBearer(string: String): Either[String, BearerToken] =
    string match
      case bearerReg(headerBase64, payloadBase64, signatureBase64) => build(headerBase64, payloadBase64, signatureBase64)
      case _                                                       => "Malformed bearer token".asLeft

  /**
    * Accepts format(s):
    * - "headerBase64.payloadBase64.signatureBase64"
    */
  def decodeToken(string: String): Either[String, BearerToken] =
    string match
      case tokenReg(headerBase64, payloadBase64, signatureBase64) => build(headerBase64, payloadBase64, signatureBase64)
      case _                                                      => "Malformed JWT token".asLeft

  /**
    * Accepts format(s):
    * - "Bearer headerBase64.payloadBase64.signatureBase64"
    * - "headerBase64.payloadBase64.signatureBase64"
    */
  def decodeBearerOrToken(string: String): Either[String, BearerToken] =
    string match
      case bearerReg(headerBase64, payloadBase64, signatureBase64) => build(headerBase64, payloadBase64, signatureBase64)
      case tokenReg(headerBase64, payloadBase64, signatureBase64)  => build(headerBase64, payloadBase64, signatureBase64)
      case _                                                       => "Malformed bearer/JWT token".asLeft

  val bearerStringCodec: StringCodec[BearerToken] = StringCodec.string.transformOption(decodeBearer(_).toOption, _.bearer)
  val tokenStringCodec: StringCodec[BearerToken] = StringCodec.string.transformOption(decodeToken(_).toOption, _.token)
  val bearerOrTokenStringCodec: StringCodec[BearerToken] = StringCodec.string.transformOption(decodeBearerOrToken(_).toOption, _.bearer)
  given StringCodec[BearerToken] = bearerStringCodec

}
