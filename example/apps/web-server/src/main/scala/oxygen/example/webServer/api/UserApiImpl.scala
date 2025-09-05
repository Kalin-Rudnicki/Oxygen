package oxygen.example.webServer.api

import oxygen.example.api.*
import oxygen.example.api.model.error.*
import oxygen.example.api.model.user.*
import oxygen.example.api.service.TokenService
import oxygen.example.conversion.apiToDomain.*
import oxygen.example.conversion.domainToApi.*
import oxygen.example.domain.model as DM
import oxygen.example.domain.service.UserService
import oxygen.http.server.CurrentRequest
import zio.*

final case class UserApiImpl(
    userService: UserService,
    tokenService: TokenService,
) extends UserApi {

  override def register(req: RegisterRequest): IO[RegistrationError, AuthResponse] =
    CurrentRequest.handle[DM.error.RegistrationError, RegistrationError] {
      for {
        user <- userService.register(req.toDomain).map(_.toApi)
        token <- tokenService.issueToken(user)
      } yield AuthResponse(user, token)
    }

  override def login(req: LoginRequest): IO[LoginError, AuthResponse] =
    CurrentRequest.handle[DM.error.LoginError, LoginError] {
      for {
        user <- userService.login(req.toDomain).map(_.toApi)
        token <- tokenService.issueToken(user)
      } yield AuthResponse(user, token)
    }

}
object UserApiImpl {

  val layer: URLayer[UserService & TokenService, UserApi] =
    ZLayer.fromFunction { UserApiImpl.apply }

}
