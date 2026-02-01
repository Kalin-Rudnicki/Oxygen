package oxygen.crypto.service

import java.time.Instant
import oxygen.crypto.model.*
import oxygen.json.*
import oxygen.json.syntax.json.*
import oxygen.predef.core.*
import zio.*

object JWTService {

  trait Validator[A] {
    def validateToken(jwt: JWT[A]): IO[SignatureError, Unit] // TODO (KR) : add support for including a `UUID => UIO[Boolean]` which checks if the token id is valid
  }
  object Validator {

    type Std[A] = Validator[JWT.StandardPayload[A]]

    def layer[A: Tag]: URLayer[BearerTokenService.Validator, JWTService.Validator[JWT.StandardPayload[A]]] =
      ZLayer.fromFunction { JWTService.Validator.StdLive.apply[A] }

    def keyLayer[A: Tag]: URLayer[HashKey.CanValidate, JWTService.Validator[JWT.StandardPayload[A]]] =
      BearerTokenService.Validator.keyLayer >>> JWTService.Validator.layer[A]
    def untypedKeyLayer[A: Tag]: URLayer[HashKey, JWTService.Validator[JWT.StandardPayload[A]]] =
      BearerTokenService.Validator.untypedKeyLayer >>> JWTService.Validator.layer[A]

    def noneLayer[A: Tag]: ULayer[JWTService.Validator[JWT.StandardPayload[A]]] =
      BearerTokenService.Validator.noneLayer >>> JWTService.Validator.layer[A]

    final case class StdLive[A](
        bearerTokenService: BearerTokenService.Validator,
    ) extends JWTService.Validator[JWT.StandardPayload[A]] {

      override def validateToken(jwt: JWT.Std[A]): IO[SignatureError, Unit] =
        bearerTokenService.validateToken(jwt.token) *>
          JWTService.validateExpiry(jwt.payload)

    }

  }

  trait Issuer[Input, Output] extends Validator[Output] {
    def issueToken(payload: Input): IO[SignatureError, JWT[Output]]
  }
  object Issuer {

    type Std[A] = Issuer[A, JWT.StandardPayload[A]]

    @jsonDiscriminator("type")
    enum ValidAfter derives JsonCodec {
      case Empty
      case Immediately
      case Delay(duration: Duration)
      case Timestamp(timestamp: Instant)

      final def resolve(now: Instant): (Boolean, Instant) = this match
        case ValidAfter.Empty                => (false, now)
        case ValidAfter.Immediately          => (true, now)
        case ValidAfter.Delay(duration)      => (true, now.plus(duration))
        case ValidAfter.Timestamp(timestamp) => (true, timestamp)

    }

    @jsonDiscriminator("type")
    enum Expiry derives JsonCodec {
      case Never
      case IssueDelay(duration: Duration)
      case ValidDelay(duration: Duration)
      case Timestamp(timestamp: Instant)

      final def resolve(now: Instant, validAfter: Instant): Option[Instant] = this match
        case Expiry.Never                => None
        case Expiry.IssueDelay(duration) => now.plus(duration).some
        case Expiry.ValidDelay(duration) => validAfter.plus(duration).some
        case Expiry.Timestamp(timestamp) => timestamp.some

    }

    final case class Config(
        validAfter: ValidAfter,
        expiry: Expiry,
    )

    def layer[A: {Tag, JsonCodec}]: URLayer[BearerTokenService.Issuer & JWTService.Issuer.Config, JWTService.Issuer[A, JWT.StandardPayload[A]]] =
      ZLayer.fromFunction { JWTService.Issuer.StdLive.apply[A] }

    def keyLayer[A: {Tag, JsonCodec}]: URLayer[HashKey.CanSign & HashKey.CanValidate & JWTService.Issuer.Config, JWTService.Issuer[A, JWT.StandardPayload[A]]] =
      BearerTokenService.Issuer.keyLayer >>> JWTService.Issuer.layer[A]
    def untypedKeyLayer[A: {Tag, JsonCodec}]: URLayer[HashKey & JWTService.Issuer.Config, JWTService.Issuer[A, JWT.StandardPayload[A]]] =
      BearerTokenService.Issuer.untypedKeyLayer >>> JWTService.Issuer.layer[A]

    def noneLayer[A: {Tag, JsonCodec}]: ULayer[JWTService.Issuer[A, JWT.StandardPayload[A]]] =
      (BearerTokenService.Issuer.noneLayer ++ ZLayer.succeed(Issuer.Config(ValidAfter.Empty, Expiry.Never))) >>> JWTService.Issuer.layer[A]

    final case class StdLive[A: JsonCodec](
        bearerTokenService: BearerTokenService.Issuer,
        config: JWTService.Issuer.Config,
    ) extends JWTService.Issuer[A, JWT.StandardPayload[A]] {

      private given codec: JsonCodec[JWT.StandardPayload[A]] = JWT.StandardPayload.derived$JsonCodec[A]

      override def issueToken(payload0: A): IO[SignatureError, JWT.Std[A]] =
        for {
          id <- Random.nextUUID
          now <- Clock.instant
          (includeValidAfter, validAfterTimestamp) = config.validAfter.resolve(now)
          expiry = config.expiry.resolve(now, validAfterTimestamp)
          payload = JWT.StandardPayload(id, now, Option.when(includeValidAfter)(validAfterTimestamp), expiry, payload0)
          token <- bearerTokenService.issueToken(payload.toJsonStringCompact)
        } yield JWT(payload, token)

      override def validateToken(jwt: JWT.Std[A]): IO[SignatureError, Unit] =
        bearerTokenService.validateToken(jwt.token) *>
          JWTService.validateExpiry(jwt.payload)

    }

  }

  private def validateExpiry(jwt: JWT.StandardPayload[?]): IO[SignatureError.NotYetValid | SignatureError.Expired, Unit] =
    for {
      now <- Clock.instant
      _ <- ZIO.foreachDiscard(jwt.validAfter) { validAfter =>
        ZIO.fail[SignatureError.NotYetValid](SignatureError.NotYetValid(now, validAfter)).whenDiscard(now.isBefore(validAfter))
      }
      _ <- ZIO.foreachDiscard(jwt.expiresAt) { expiresAt =>
        ZIO.fail[SignatureError.Expired](SignatureError.Expired(now, expiresAt)).unlessDiscard(now.isBefore(expiresAt))
      }
    } yield ()

}
