package oxygen.crypto.service

import oxygen.crypto.model.*
import oxygen.predef.json.*
import zio.*

object JWTService {

  trait Validator[A] {
    def validateToken(jwt: JWT[A]): IO[TokenError, Unit]
  }
  object Validator {

    type Std[A] = Validator[JWT.StandardPayload[A]]

    def layer[A: Tag]: URLayer[BearerTokenService.Validator, JWTService.Validator[JWT.StandardPayload[A]]] =
      ZLayer.fromFunction { JWTService.Validator.StdLive.apply[A] }

    def configLayer[A: Tag]: URLayer[Key.CanValidate.Config, JWTService.Validator[JWT.StandardPayload[A]]] =
      BearerTokenService.Validator.configLayer >>> JWTService.Validator.layer[A]

    final case class StdLive[A](
        bearerTokenService: BearerTokenService.Validator,
    ) extends JWTService.Validator[JWT.StandardPayload[A]] {

      override def validateToken(jwt: JWT.Std[A]): IO[TokenError, Unit] =
        bearerTokenService.validateToken(jwt.token) *>
          JWTService.validateExpiry(jwt.payload)

    }

  }

  trait Issuer[Input, Output] extends Validator[Output] {
    def issueToken(payload: Input): IO[TokenError.CryptoFailure, JWT[Output]]
  }
  object Issuer {

    type Std[A] = Issuer[A, JWT.StandardPayload[A]]

    final case class Config(timeToLive: Duration)

    def layer[A: {JsonCodec, Tag}]: URLayer[BearerTokenService.Issuer & JWTService.Issuer.Config, JWTService.Issuer[A, JWT.StandardPayload[A]]] =
      ZLayer.fromFunction { JWTService.Issuer.StdLive.apply[A] }

    def configLayer[A: {JsonCodec, Tag}]: URLayer[Key.CanIssue.Config & JWTService.Issuer.Config, JWTService.Issuer[A, JWT.StandardPayload[A]]] =
      BearerTokenService.Issuer.configLayer >>> JWTService.Issuer.layer[A]

    final case class StdLive[A: JsonCodec](
        bearerTokenService: BearerTokenService.Issuer,
        config: JWTService.Issuer.Config,
    ) extends JWTService.Issuer[A, JWT.StandardPayload[A]] {

      private given codec: JsonCodec[JWT.StandardPayload[A]] = JWT.StandardPayload.derived$JsonCodec[A]

      override def issueToken(payload0: A): IO[TokenError.CryptoFailure, JWT.Std[A]] =
        for {
          id <- Random.nextUUID
          now <- Clock.instant
          payload = JWT.StandardPayload(id, now, now.plus(config.timeToLive), payload0)
          token <- bearerTokenService.issueToken(payload.toJsonStringCompact)
        } yield JWT(payload, token)

      override def validateToken(jwt: JWT.Std[A]): IO[TokenError, Unit] =
        bearerTokenService.validateToken(jwt.token) *>
          JWTService.validateExpiry(jwt.payload)

    }

  }

  private def validateExpiry(jwt: JWT.StandardPayload[?]): IO[TokenError.Expired, Unit] =
    for {
      now <- Clock.instant
      expiresAt = jwt.expiresAt
      _ <- ZIO.fail[TokenError.Expired](TokenError.Expired(now, expiresAt)).unlessDiscard(now.isBefore(expiresAt))
    } yield ()

}
