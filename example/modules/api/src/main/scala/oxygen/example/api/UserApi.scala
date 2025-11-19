package oxygen.example.api

import oxygen.example.api.model.error.*
import oxygen.example.api.model.user.*
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import zio.*

trait UserApi derives DeriveClient {

  @route.post("/api/user/register")
  def register(
      @param.body req: RegisterRequest,
  ): IO[RegistrationError, AuthResponse]

  @route.post("/api/user/login")
  def login(
      @param.body req: LoginRequest,
  ): IO[LoginError, AuthResponse]

  @route.get("/api/make-response")
  def makeResponse(
      @param.query size: Int,
  ): UIO[String]

}
object UserApi {

  def register(
      req: RegisterRequest,
  ): ZIO[UserApi, RegistrationError, AuthResponse] =
    ZIO.serviceWithZIO[UserApi](_.register(req))

  def login(
      req: LoginRequest,
  ): ZIO[UserApi, LoginError, AuthResponse] =
    ZIO.serviceWithZIO[UserApi](_.login(req))

}
