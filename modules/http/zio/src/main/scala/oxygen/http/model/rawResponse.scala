package oxygen.http.model

import oxygen.http.core.*
import oxygen.http.schema.ExpectedStatuses
import zio.http.*

final case class RawResponse(
    status: Status,
    headers: Headers,
    body: ReadOnlyCachedHttpBody,
)
object RawResponse {
  given ResponseCodec[RawResponse] =
    ResponseCodec.Raw(ExpectedStatuses.All).transform(RawResponse.apply, r => (r.status, r.headers, r.body))
}

final case class RawSuccessResponse(
    status: Status,
    headers: Headers,
    body: ReadOnlyCachedHttpBody,
)
object RawSuccessResponse {
  given ResponseCodec[RawSuccessResponse] =
    ResponseCodec.Raw(ExpectedStatuses.StatusRange.Success).transform(RawSuccessResponse.apply, r => (r.status, r.headers, r.body))
}

final case class RawErrorResponse(
    status: Status,
    headers: Headers,
    body: ReadOnlyCachedHttpBody,
)
object RawErrorResponse {
  given ResponseCodec[RawErrorResponse] =
    ResponseCodec.Raw(ExpectedStatuses.StatusRange.Error).transform(RawErrorResponse.apply, r => (r.status, r.headers, r.body))
}
