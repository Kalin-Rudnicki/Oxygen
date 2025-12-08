package oxygen.example.api.model.error

import oxygen.http.client.ClientErrorHandler
import oxygen.http.core.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import zio.StackTrace

enum LoginError extends Throwable derives StatusCodes, JsonSchema {

  @statusCode.Unauthorized case InvalidCredentials

  @statusCode.BadRequest case DecodingFailure(message: String)

  @statusCode.InternalServerError case InternalServerError(error: Option[InternalError])

}
object LoginError {
  given ClientErrorHandler[LoginError] =
    new ClientErrorHandler[LoginError] {
      override def wrapDeath(error: Throwable, trace: StackTrace): Option[LoginError] = None
      override def wrapDecodingFailure(error: ResponseDecodingFailure): Option[LoginError] = DecodingFailure(error.getMessage).some
    }
}
