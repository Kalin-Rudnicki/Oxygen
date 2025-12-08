package oxygen.example.api.model.error

import oxygen.http.client.ClientErrorHandler
import oxygen.http.core.*
import oxygen.schema.JsonSchema

enum UIApiError extends Throwable derives StatusCodes, JsonSchema {

  @statusCode.NotFound case ResourceNotFound(resource: String)

  @statusCode.BadRequest case BadRequest(message: String)

}
object UIApiError {
  given ClientErrorHandler[UIApiError] = ClientErrorHandler.notHandled
}
