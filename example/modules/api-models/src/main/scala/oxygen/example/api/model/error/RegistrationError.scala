package oxygen.example.api.model.error

import oxygen.example.core.model.user.{*, given}
import oxygen.http.client.ClientErrorHandler
import oxygen.http.core.*
import oxygen.schema.JsonSchema

enum RegistrationError extends Throwable derives StatusCodes, JsonSchema {

  @statusCode.Conflict case EmailAlreadyExists(email: Email)

  @statusCode.BadRequest case DecodingFailure(message: String)

  @statusCode.InternalServerError case InternalServerError(error: Option[InternalError])

}
object RegistrationError {
  given ClientErrorHandler[RegistrationError] = ClientErrorHandler.notHandled
}
