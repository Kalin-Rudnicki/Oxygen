package oxygen.crypto.service

import oxygen.crypto.model.*
import zio.*

object BearerTokenService {

  trait Validator {
    def validateToken(token: BearerToken): IO[JWTError, Unit]
  }
  object Validator {

    def layer(secret: HashKey.CanValidate): ULayer[BearerTokenService.Validator] =
      ZLayer.succeed { BearerTokenService.Validator.Live(secret) }

    def secretLayer: URLayer[HashKey.CanValidate, BearerTokenService.Validator] =
      ZLayer.fromFunction { BearerTokenService.Validator.Live.apply }

    def configLayer: URLayer[HashKey.CanValidate.Config, BearerTokenService.Validator] =
      ZLayer.service[HashKey.CanValidate.Config].project(_.secret) >>> secretLayer

    def noneLayer: ULayer[BearerTokenService.Validator] =
      BearerTokenService.Validator.layer(HashKey.none)

    final case class Live(secret: HashKey.CanValidate) extends BearerTokenService.Validator {

      override def validateToken(token: BearerToken): IO[JWTError, Unit] =
        attemptCrypto { secret.validate(token) }.absolve

    }

  }

  trait Issuer extends Validator {
    def issueToken(payload: String): IO[JWTError.CryptoFailure, BearerToken]
  }
  object Issuer {

    def layer(secret: HashKey.CanIssue): ULayer[BearerTokenService.Issuer] =
      ZLayer.succeed { BearerTokenService.Issuer.Live(secret) }

    def secretLayer: URLayer[HashKey.CanIssue, BearerTokenService.Issuer] =
      ZLayer.fromFunction { BearerTokenService.Issuer.Live.apply }

    def configLayer: URLayer[HashKey.CanIssue.Config, BearerTokenService.Issuer] =
      ZLayer.service[HashKey.CanIssue.Config].project(_.secret) >>> secretLayer

    def noneLayer: ULayer[BearerTokenService.Issuer] =
      BearerTokenService.Issuer.layer(HashKey.none)

    final case class Live(secret: HashKey.CanIssue) extends BearerTokenService.Issuer {

      override def validateToken(token: BearerToken): IO[JWTError, Unit] =
        attemptCrypto { secret.validate(token) }.absolve

      override def issueToken(payload: String): IO[JWTError.CryptoFailure, BearerToken] =
        attemptCrypto { secret.issue(payload) }

    }

  }

  private def attemptCrypto[A](effect: => A): IO[JWTError.CryptoFailure, A] =
    ZIO.attempt { effect }.mapError(JWTError.CryptoFailure(_))

}
