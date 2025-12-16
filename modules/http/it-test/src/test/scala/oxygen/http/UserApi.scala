package oxygen.http

import java.time.Instant
import java.util.UUID
import oxygen.http.client.{ClientErrorHandler, DeriveClient}
import oxygen.http.core.{*, given}
import oxygen.http.server.{DeriveEndpoints, ServerErrorConfig, ServerErrorHandler}
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.zio.ExtractedCauses
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

final case class UserEvent(
    userId: UUID,
    message: String,
) derives JsonSchema

final case class CustomPathItem(value: String)
object CustomPathItem {

  given RequestCodec.PathLike[CustomPathItem] =
    ("custom" / "path" / RequestCodec.path.plain[String]("value")).autoTransform[CustomPathItem]

}

enum ApiError derives JsonSchema, StatusCodes {

  @statusCode.`404` case NoSuchUser(id: UUID)

  @statusCode.`400` case RequestDecodingFailure(error: oxygen.http.core.RequestDecodingFailure)
  @statusCode.`400` case ResponseDecodingFailure(error: oxygen.http.core.ResponseDecodingFailure)

  @statusCode.`500` case InternalError(message: Option[String])

}
object ApiError {

  given ServerErrorHandler[ApiError] =
    new ServerErrorHandler[ApiError] {
      override def convertCause(cause: ExtractedCauses[oxygen.http.core.RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[ApiError] =
        cause match
          case ExtractedCauses.Failures(failures, _, _)                                   => ApiError.RequestDecodingFailure(failures.head.value).some
          case ExtractedCauses.Defects(defects, _) if errorConfig.exposeInternalErrors    => ApiError.InternalError(defects.head.value.safeGetMessage.some).some
          case ExtractedCauses.Interrupts(interrupts) if errorConfig.exposeInternalErrors => ApiError.InternalError(s"Interrupted by fiberId=${interrupts.head.fiberId}".some).some
          case _                                                                          => ApiError.InternalError(None).some
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
trait UserApi {

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

  @route.get("/user/%/events")
  def userEvents(
      @param.path userId: UUID,
      @param.query numEvents: Option[Int],
  ): ServerSentEvents[String, UserEvent]

  @route.get("/abc/%/ghi")
  def macroTest(
      @param.path.custom value: CustomPathItem,
      @param.query instant: Option[Instant],
      @param.query limit: Option[Int],
      @param.header authorization: String,
  ): IO[String, String]

}
object UserApi {

  given DeriveEndpoints[UserApi] = DeriveEndpoints.derived
  given DeriveClient[UserApi] = DeriveClient.derived

}
