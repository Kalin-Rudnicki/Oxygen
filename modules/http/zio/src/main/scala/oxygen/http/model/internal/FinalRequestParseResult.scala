package oxygen.http.model.internal

import oxygen.http.core.*
import zio.*

enum FinalRequestParseResult[+A] {
  case Success(value: A)
  case NotFound
  case Error(error: RequestDecodingFailure)
  case Effect(effect: ZIO[Scope, Option[RequestDecodingFailure], A])
}
