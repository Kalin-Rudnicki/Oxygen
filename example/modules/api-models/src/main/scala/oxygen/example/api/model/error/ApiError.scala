package oxygen.example.api.model.error

import oxygen.http.core.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

enum ApiError extends Throwable derives StatusCodes, JsonSchema {

  @statusCode.Unauthorized case Unauthorized(message: String, userMessage: Option[String])
  @statusCode.Unauthorized case InvalidToken(message: Option[String])

  @statusCode.Forbidden case Forbidden(message: String, userMessage: Option[String])

  @statusCode.Conflict case Conflict(message: String, userMessage: Option[String])

  @statusCode.NotFound case NotFound(message: String, userMessage: Option[String])

  @statusCode.BadRequest case DecodingFailure(message: String)

  @statusCode.InternalServerError case InternalServerError(error: Option[InternalError])

}
object ApiError {

  sealed abstract class ErrorBuilder[E](make: (String, Option[String]) => E) {
    def apply(message: String, userMessage: Option[String]): E = make(message, userMessage)
    def userSafe(userMessage: String): E = make(userMessage, userMessage.some)
    def noUserMessage(message: String): E = make(message, None)
    def withUserMessage(message: String, userMessage: String): E = make(message, userMessage.some)
  }

  object Unauthorized extends ErrorBuilder[Unauthorized](new Unauthorized(_, _))
  object Forbidden extends ErrorBuilder[Forbidden](new Forbidden(_, _))
  object Conflict extends ErrorBuilder[Conflict](new Conflict(_, _))
  object NotFound extends ErrorBuilder[NotFound](new NotFound(_, _))

}
