package oxygen.crypto.service

import java.nio.charset.StandardCharsets
import oxygen.crypto.model.*
import oxygen.json.syntax.json.*
import zio.*

object BearerTokenService {

  trait Validator {
    def validateToken(token: BearerToken): IO[SignatureError, Unit]
  }
  object Validator {

    def layer: URLayer[SignatureService.Validator, BearerTokenService.Validator] = ZLayer.fromFunction { BearerTokenService.Validator.Live.apply }

    def keyLayer: URLayer[HashKey.CanValidate, BearerTokenService.Validator] = SignatureService.Validator.live >>> layer
    def untypedKeyLayer: URLayer[HashKey, BearerTokenService.Validator] = SignatureService.Validator.untypedLive >>> layer

    def noneLayer: ULayer[BearerTokenService.Validator] = ZLayer.succeed(HashKey.Noop) >>> keyLayer

    final case class Live(
        validator: SignatureService.Validator,
    ) extends BearerTokenService.Validator {

      override def validateToken(token: BearerToken): IO[SignatureError, Unit] =
        validator.validateBase64(token.`headerBase64.payloadBase64`, Signature.Base64(token.signatureBase64))

    }

  }

  trait Issuer extends Validator {
    def issueToken(payload: String): UIO[BearerToken]
  }
  object Issuer {

    def layer: URLayer[SignatureService.Signer & SignatureService.Validator, BearerTokenService.Issuer] = ZLayer.fromFunction { BearerTokenService.Issuer.Live.apply }

    def keyLayer: URLayer[HashKey.CanSign & HashKey.CanValidate, BearerTokenService.Issuer] = SignatureService.live >>> layer
    def untypedKeyLayer: URLayer[HashKey, BearerTokenService.Issuer] = SignatureService.untypedLive >>> layer

    def noneLayer: ULayer[BearerTokenService.Issuer] = ZLayer.succeed(HashKey.Noop) >>> keyLayer

    final case class Live(
        signer: SignatureService.Signer,
        validator: SignatureService.Validator,
    ) extends BearerTokenService.Issuer {

      override def validateToken(token: BearerToken): IO[SignatureError, Unit] =
        ZIO.fail(SignatureError.InvalidAlgorithm(validator.alg, token.header.alg)).unlessDiscard(validator.alg == token.header.alg) *>
          validator.validateBase64(token.`headerBase64.payloadBase64`, Signature.Base64(token.signatureBase64))

      override def issueToken(payload: String): UIO[BearerToken] = {
        val header: JWTHeader = JWTHeader(signer.alg, JWTHeader.Type.JWT)
        val headerStringPlain: String = header.toJsonStringCompact
        val headerStringBase64: String = Base64.urlEncoder.encodeToString(headerStringPlain.getBytes(StandardCharsets.UTF_8))
        val payloadBase64: String = Base64.urlEncoder.encodeToString(payload.getBytes(StandardCharsets.UTF_8))
        for {
          signature <- signer.signBase64(s"$headerStringBase64.$payloadBase64")
        } yield BearerToken(
          headerBase64 = headerStringBase64,
          header = header,
          payloadBase64 = payloadBase64,
          payload = payload,
          signatureBase64 = signature.bytes,
        )
      }

    }

  }

}
