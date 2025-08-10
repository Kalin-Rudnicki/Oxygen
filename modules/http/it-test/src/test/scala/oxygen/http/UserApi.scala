package oxygen.http

import java.util.UUID
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import oxygen.http.model.{DecodingFailure as OopsDecode, *}
import oxygen.http.server.{DeriveEndpoints, ServerErrorHandler}
import oxygen.json.*
import oxygen.predef.core.*
import scala.annotation.experimental
import zio.*

final case class User(
    id: UUID,
    first: String,
    last: String,
    age: Int,
) derives JsonCodec

final case class CreateUser(
    first: String,
    last: String,
    age: Int,
) derives JsonCodec

enum ApiError derives JsonCodec {

  @httpCode(HttpCode.NotFound) case NoSuchUser(id: UUID)

  @httpCode(HttpCode.BadRequest) case DecodingFailure(error: OopsDecode)

  @httpCode(HttpCode.InternalServerError) case InternalError(message: Option[String])

}
object ApiError {

  given ServerErrorHandler[ApiError] =
    new ServerErrorHandler[ApiError] {
      override def wrapDeath(error: Throwable, exposeInternalErrors: Boolean): Option[ApiError] =
        InternalError(Option.when(exposeInternalErrors)(error.safeGetMessage)).some
      override def wrapDecodingFailure(error: OopsDecode): Option[ApiError] =
        DecodingFailure(error).some
    }

}

@experimental
trait UserApi derives DeriveEndpoints, DeriveClient {

  @route.get("/user/%")
  def userById(
      @param.path id: UUID,
  ): IO[ApiError, User]

  @route.get("/user")
  def allUsers(): UIO[Contiguous[User]]

  @route.post("/user")
  def createUser(
      @param.body.json create: CreateUser,
  ): UIO[User]

}
