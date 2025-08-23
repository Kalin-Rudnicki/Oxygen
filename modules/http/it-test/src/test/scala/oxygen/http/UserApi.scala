package oxygen.http

import java.util.UUID
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import oxygen.http.server.{DeriveEndpoints, ServerErrorHandler}
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.zio.instances.chunkSeqOps
import scala.annotation.experimental
import zio.*

final case class User(
    id: UUID,
    first: String,
    last: String,
    age: Int,
) derives JsonSchema

final case class CreateUser(
    first: String,
    last: String,
    age: Int,
) derives JsonSchema

enum ApiError derives JsonSchema, StatusCodes {

  @statusCode.`404` case NoSuchUser(id: UUID)

  @statusCode.`400` case DecodingFailure(error: RequestDecodingFailure)

  @statusCode.`500` case InternalError(message: Option[String])

}
object ApiError {

  given ServerErrorHandler[ApiError] =
    new ServerErrorHandler[ApiError] {
      override def wrapDeath(error: Throwable, exposeInternalErrors: Boolean): Option[ApiError] =
        InternalError(Option.when(exposeInternalErrors)(error.safeGetMessage)).some
      override def wrapDecodingFailure(error: RequestDecodingFailure): Option[ApiError] =
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
  def allUsers(): UIO[Chunk[User]]

  @route.post("/user")
  def createUser(
      @param.body.json create: CreateUser,
  ): UIO[User]

}
