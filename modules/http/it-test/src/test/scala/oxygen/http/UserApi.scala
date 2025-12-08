package oxygen.http

import java.util.UUID
import oxygen.http.client.{ClientErrorHandler, DeriveClient}
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

  @statusCode.`400` case RequestDecodingFailure(error: oxygen.http.core.RequestDecodingFailure)
  @statusCode.`400` case ResponseDecodingFailure(error: oxygen.http.core.ResponseDecodingFailure)

  @statusCode.`500` case InternalError(message: Option[String])

}
object ApiError {

  given ServerErrorHandler[ApiError] =
    new ServerErrorHandler[ApiError] {
      override def wrapDeath(error: Throwable, trace: StackTrace, exposeInternalErrors: Boolean): Option[ApiError] =
        InternalError(Option.when(exposeInternalErrors)(error.safeGetMessage)).some
      override def wrapDecodingFailure(error: oxygen.http.core.RequestDecodingFailure): Option[ApiError] =
        RequestDecodingFailure(error).some
    }

  given ClientErrorHandler[ApiError] =
    new ClientErrorHandler[ApiError] {
      override def wrapDeath(error: Throwable, trace: StackTrace): Option[ApiError] =
        None
      override def wrapDecodingFailure(error: oxygen.http.core.ResponseDecodingFailure): Option[ApiError] =
        ResponseDecodingFailure(error).some
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

  @route.get("/user/search")
  def userSearch(
      @param.query firstName: Option[String] = None,
      @param.query lastName: Option[String] = None,
  ): UIO[Set[User]]

}
