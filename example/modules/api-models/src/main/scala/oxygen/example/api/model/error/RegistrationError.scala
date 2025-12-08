package oxygen.example.api.model.error

import oxygen.example.core.model.user.{*, given}
import oxygen.http.client.ClientErrorHandler
import oxygen.http.core.*
import oxygen.http.model.ServerErrors
import oxygen.schema.JsonSchema

enum RegistrationError extends Throwable derives StatusCodes, JsonSchema {

  @statusCode.Conflict case EmailAlreadyExists(email: Email)

  @statusCode.BadRequest case DecodingFailure(message: String)

  @statusCode.InternalServerError case InternalServerError(error: Option[ServerErrors])

}
object RegistrationError {
  given ClientErrorHandler[RegistrationError] = ClientErrorHandler.notHandled
}
