package oxygen.http.core

import oxygen.core.syntax.throwable.safeGetMessage
import oxygen.json.*
import oxygen.schema.JsonSchema
import oxygen.schema.instances.throwable.encoded

enum DecodingFailureCause derives JsonSchema {
  case MissingRequired
  case ManyNotSupported(received: Int)
  case DecodeError(error: String)
  case ExecutionFailure(cause: Throwable)

  override final def toString: String = this match
    case DecodingFailureCause.MissingRequired            => "Missing required parameter"
    case DecodingFailureCause.ManyNotSupported(received) => s"Providing many parameters is not allowed (received $received)"
    case DecodingFailureCause.DecodeError(error)         => s"[failed to decode] $error"
    case DecodingFailureCause.ExecutionFailure(cause)    => s"[failed to execute]: ${cause.safeGetMessage}"

}
