package oxygen.crypto.service

import oxygen.crypto.model.*
import zio.*

object BearerTokenService {

  trait Validator {
    def validateToken(token: BearerToken): IO[TokenError, Unit]
  }
  object Validator {

    def layer(secret: Key.CanValidate): ULayer[BearerTokenService.Validator] =
      ZLayer.succeed { BearerTokenService.Validator.Live(secret) }

    def secretLayer: URLayer[Key.CanValidate, BearerTokenService.Validator] =
      ZLayer.fromFunction { BearerTokenService.Validator.Live.apply }

    def configLayer: URLayer[Key.CanValidate.Config, BearerTokenService.Validator] =
      ZLayer.service[Key.CanValidate.Config].project(_.secret) >>> secretLayer

    def noneLayer: ULayer[BearerTokenService.Validator] =
      BearerTokenService.Validator.layer(Key.none)

    final case class Live(secret: Key.CanValidate) extends BearerTokenService.Validator {

      override def validateToken(token: BearerToken): IO[TokenError, Unit] =
        attemptCrypto { secret.validate(token) }.absolve

    }

  }

  trait Issuer extends Validator {
    def issueToken(payload: String): IO[TokenError.CryptoFailure, BearerToken]
  }
  object Issuer {

    def layer(secret: Key.CanIssue): ULayer[BearerTokenService.Issuer] =
      ZLayer.succeed { BearerTokenService.Issuer.Live(secret) }

    def secretLayer: URLayer[Key.CanIssue, BearerTokenService.Issuer] =
      ZLayer.fromFunction { BearerTokenService.Issuer.Live.apply }

    def configLayer: URLayer[Key.CanIssue.Config, BearerTokenService.Issuer] =
      ZLayer.service[Key.CanIssue.Config].project(_.secret) >>> secretLayer

    def noneLayer: ULayer[BearerTokenService.Issuer] =
      BearerTokenService.Issuer.layer(Key.none)

    final case class Live(secret: Key.CanIssue) extends BearerTokenService.Issuer {

      override def validateToken(token: BearerToken): IO[TokenError, Unit] =
        attemptCrypto { secret.validate(token) }.absolve

      override def issueToken(payload: String): IO[TokenError.CryptoFailure, BearerToken] =
        attemptCrypto { secret.issue(payload) }

    }

  }

  private def attemptCrypto[A](effect: => A): IO[TokenError.CryptoFailure, A] =
    ZIO.attempt { effect }.mapError(TokenError.CryptoFailure(_))

}
