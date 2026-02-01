package oxygen.example.api.service

import oxygen.crypto.model.SignatureError
import oxygen.crypto.service.*
import oxygen.example.api.model.error.ApiError
import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.conversion.apiToDomain.*
import oxygen.example.domain.model.user.SimpleUser
import oxygen.predef.core.*
import zio.*

trait TokenService {

  def issueToken(user: User): UIO[UserToken]

  def validateToken(token: UserToken): ZIO[Scope, ApiError, SimpleUser]

}
object TokenService {

  val layer: URLayer[JWTService.Issuer.Std[User], TokenService] =
    ZLayer.fromFunction { Live.apply }

  final case class Live(jwtService: JWTService.Issuer.Std[User]) extends TokenService {

    override def issueToken(user: User): UIO[UserToken] =
      jwtService.issueToken(user).orDie.map(UserToken(_))

    override def validateToken(token: UserToken): ZIO[Scope, ApiError, SimpleUser] =
      jwtService
        .validateToken(token.jwt)
        .flatMapError {
          case _: SignatureError.InvalidAlgorithm => ZIO.succeed(ApiError.InvalidToken(None))
          case SignatureError.InvalidSignature    => ZIO.succeed(ApiError.InvalidToken(None))
          case _: SignatureError.Expired          => ZIO.succeed(ApiError.InvalidToken("Expired".some))
          case e                                  => ZIO.die(e)
        }
        .as(token.user.toDomain)
        .tap { user => ZIO.logAnnotateScoped("userId", user.id.id.toString) }

  }

}
