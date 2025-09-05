package oxygen.example.api.model.error

import oxygen.http.core.*
import oxygen.schema.JsonSchema

enum LoginError extends Throwable derives StatusCodes, JsonSchema {

  @statusCode.Unauthorized case InvalidCredentials

  @statusCode.BadRequest case DecodingFailure(message: String)

  @statusCode.InternalServerError case InternalServerError(error: Option[InternalError])

}
