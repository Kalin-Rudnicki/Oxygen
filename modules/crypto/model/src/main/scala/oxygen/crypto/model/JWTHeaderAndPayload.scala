package oxygen.crypto.model

import oxygen.json.JsonEncoder
import oxygen.json.syntax.json.*

/**
  * The base64url-encoded header + payload of a JWT — i.e. the exact bytes that get signed — split out as
  * a first-class value so a caller can sign a **custom** claim payload and then reassemble a full [[JWT]].
  *
  * The JWS signing input is `headerBase64.payloadBase64` (see [[`headerBase64.payloadBase64`]]): feed it to
  * [[oxygen.crypto.service.SignatureService.Signer.signBase64]] to get a [[Signature.Base64]], then call
  * [[withSignature]] to produce the finished `JWT[A]`. This is the low-level counterpart to
  * `BearerTokenService.Issuer.issueToken(jsonString)` — for callers (e.g. an OAuth authorization server
  * minting audience-bound access tokens) that need to control the payload shape and the signature step
  * themselves rather than accept oxygen's opinionated `JWT.StandardPayload` issuance.
  */
final case class JWTHeaderAndPayload[A] private (
    headerBase64: Bytes.UrlBase64,
    header: JWTHeader,
    payloadBase64: Bytes.UrlBase64,
    payloadPlain: String,
    payload: A,
) {

  /** The JWS signing input: `headerBase64.payloadBase64` — the value the signature is computed over. */
  def `headerBase64.payloadBase64`: String = s"${headerBase64.unwrap}.${payloadBase64.unwrap}"

  /**
    * Attach a signature — as produced by `Signer.signBase64` over [[`headerBase64.payloadBase64`]] — to
    * produce a complete `JWT[A]` (payload + assembled `BearerToken`).
    */
  def withSignature(signature: Signature.Base64): JWT[A] =
    JWT(
      payload,
      BearerToken(
        headerBase64 = headerBase64,
        header = header,
        payloadBase64 = payloadBase64,
        payload = payloadPlain,
        signatureBase64 = signature,
      ),
    )

}
object JWTHeaderAndPayload {

  /** Base64url-encode `header` and `payload` into the signable [[JWTHeaderAndPayload]] (no signature yet). */
  def calculate[A: JsonEncoder](header: JWTHeader, payload: A): JWTHeaderAndPayload[A] = {
    val headerPlain: String = header.toJsonStringCompact
    val payloadPlain: String = payload.toJsonStringCompact
    JWTHeaderAndPayload(
      headerBase64 = Bytes.Raw.stringBytes(headerPlain).urlBase64,
      header = header,
      payloadBase64 = Bytes.Raw.stringBytes(payloadPlain).urlBase64,
      payloadPlain = payloadPlain,
      payload = payload,
    )
  }

}
