package oxygen.http.api

import oxygen.http.client.{ClientErrorHandler, DeriveClient}
import oxygen.http.core.*
import oxygen.http.model.*
import oxygen.json.jsonDiscriminator
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import scala.annotation.experimental
import zio.*

@experimental
trait ResourceApi derives DeriveClient {

  @route.get("/res/%")
  def resource(@param.path rest: List[String]): IO[ResourceApi.ApiError, RawSuccessResponse]

}
object ResourceApi {

  @jsonDiscriminator("type")
  sealed trait ApiError extends Error derives JsonSchema, StatusCodes
  object ApiError {

    @statusCode.BadRequest
    final case class MalformedPath(path: String, message: String) extends ApiError {
      override def errorMessage: Text = str"MalformedPath[$path]: $message"
    }

    @statusCode.NotFound
    final case class NoSuchPath(path: String, message: Option[String]) extends ApiError {
      override def errorMessage: Text = message match
        case Some(message) => str"NoSuchPath[$path]: $message"
        case None          => str"NoSuchPath[$path]"
    }

    given ClientErrorHandler[ApiError] = ClientErrorHandler.notHandled

  }

}
