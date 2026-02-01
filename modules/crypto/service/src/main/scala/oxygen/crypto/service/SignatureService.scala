package oxygen.crypto.service

import java.nio.charset.StandardCharsets
import oxygen.crypto.model.*
import oxygen.predef.core.*
import scala.reflect.TypeTest
import zio.*

object SignatureService {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Signer
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Signer {

    val alg: JWTHeader.Alg

    /**
      * Accepts a String, and signs the underlying bytes.
      * The output is the raw signature bytes.
      */
    def signRaw(value: String): UIO[Signature.Raw]

    /**
      * Accepts a String, and signs the underlying bytes.
      * The output is the base-64 string representation of the signature bytes.
      */
    final def signBase64(value: String): UIO[Signature.Base64] =
      for {
        rawSignature <- signRaw(value)
        base64Signature <- attemptSign { rawSignature.toBase64 }
      } yield base64Signature

    /**
      * Accepts a String, converts the string to base-64, and signs the underlying bytes of that base-64 string.
      * The output is the base-64 string representation of the signature bytes.
      */
    final def base64AndSign(value: String): UIO[(Bytes.UrlBase64, Signature.Base64)] =
      for {
        base64Value <- attemptSign { Bytes.Raw(value.getBytes(StandardCharsets.UTF_8)).urlBase64 }
        base64Signature <- signBase64(base64Value)
      } yield (base64Value, base64Signature)

  }
  object Signer {

    final class Live(key: HashKey.CanSign) extends Signer {

      override val alg: JWTHeader.Alg = key.alg
      override def signRaw(value: String): UIO[Signature.Raw] = attemptSign { key.signRaw(value) }

    }

    def live: URLayer[HashKey.CanSign, SignatureService.Signer] = ZLayer.fromFunction { SignatureService.Signer.Live.apply }
    def untypedLive: URLayer[HashKey, SignatureService.Signer] = SignatureService.narrowKeyLayer[HashKey.CanSign] >>> live

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Signer
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Validator {

    val alg: JWTHeader.Alg

    def isValidRaw(value: String, signature: Signature.Raw): UIO[Boolean]

    final def validateRaw(value: String, signature: Signature.Raw): IO[SignatureError.InvalidSignature.type, Unit] =
      isValidRaw(value, signature).flatMap {
        case true  => ZIO.unit
        case false => ZIO.fail(SignatureError.InvalidSignature)
      }

    def isValidBase64(value: String, signature: Signature.Base64): UIO[Boolean]

    final def validateBase64(value: String, signature: Signature.Base64): IO[SignatureError.InvalidSignature.type, Unit] =
      isValidBase64(value, signature).flatMap {
        case true  => ZIO.unit
        case false => ZIO.fail(SignatureError.InvalidSignature)
      }

  }
  object Validator {

    final class Live(key: HashKey.CanValidate) extends Validator {

      override val alg: JWTHeader.Alg = key.alg
      override def isValidRaw(value: String, signature: Signature.Raw): UIO[Boolean] = attemptValidate { key.validateRaw(value, signature) }
      override def isValidBase64(value: String, signature: Signature.Base64): UIO[Boolean] = attemptValidate { key.validateBase64(value, signature) }

    }

    def live: URLayer[HashKey.CanValidate, SignatureService.Validator] = ZLayer.fromFunction { SignatureService.Validator.Live.apply }
    def untypedLive: URLayer[HashKey, SignatureService.Validator] = SignatureService.narrowKeyLayer[HashKey.CanValidate] >>> live

  }

  def live: URLayer[HashKey.CanSign & HashKey.CanValidate, SignatureService.Signer & SignatureService.Validator] = Signer.live ++ Validator.live
  def untypedLive: URLayer[HashKey, SignatureService.Signer & SignatureService.Validator] = Signer.untypedLive ++ Validator.untypedLive

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def attemptSign[A](thunk: => A)(using Trace): UIO[A] =
    ZIO.attempt { thunk }.mapError(SignatureError.GenericSigningFailure(_)).!

  private def attemptValidate[A](thunk: => A)(using Trace): UIO[A] =
    ZIO.attempt { thunk }.mapError(SignatureError.GenericValidationFailure(_)).!

  private def narrowKeyLayer[A <: HashKey: {TypeTag as tt, Tag}](using typeTest: TypeTest[HashKey, A]): URLayer[HashKey, A] =
    ZLayer {
      ZIO.serviceWithZIO[HashKey] {
        case typeTest(key) => ZIO.succeed(key)
        case key           => ZIO.dieMessage(s"Invalid key type, [expected=${tt.prefixObject}] but got [actual=${TypeTag.fromClass(key.getClass).prefixObject}]")
      }
    }

}
