package oxygen.http.model

import oxygen.http.core.*
import oxygen.http.core.ZioHttpCompat.optMediaType
import oxygen.http.schema.ExpectedStatuses
import zio.http.*

final case class RawResponse(
    status: Status,
    headers: Headers,
    body: Body,
)
object RawResponse {
  given ResponseCodec[RawResponse] =
    ResponseCodec.Raw(ExpectedStatuses.All).transform(RawResponse.apply, r => (r.status, r.headers, r.body))
}

final case class RawSuccessResponse(
    status: Status,
    headers: Headers,
    body: Body,
)
object RawSuccessResponse {
  given ResponseCodec[RawSuccessResponse] =
    ResponseCodec.Raw(ExpectedStatuses.StatusRange.Success).transform(RawSuccessResponse.apply, r => (r.status, r.headers, r.body))
}

final case class RawErrorResponse(
    status: Status,
    headers: Headers,
    body: Body,
)
object RawErrorResponse {
  given ResponseCodec[RawErrorResponse] =
    ResponseCodec.Raw(ExpectedStatuses.StatusRange.Error).transform(RawErrorResponse.apply, r => (r.status, r.headers, r.body))
}

final case class RawResponseText(
    status: Status,
    headers: Headers,
    body: String,
    contentType: Option[MediaType],
)
object RawResponseText {
  given ResponseCodec[RawResponseText] =
    ResponseCodec
      .Raw(ExpectedStatuses.All)
      .transformZIO(
        (status, headers, body) => body.asString.orDie.map(RawResponseText(status, headers, _, body.mediaType)),
        r => (r.status, r.headers, Body.fromString(r.body).optMediaType(r.contentType)),
      )
}

final case class RawSuccessResponseText(
    status: Status,
    headers: Headers,
    body: String,
    contentType: Option[MediaType],
)
object RawSuccessResponseText {
  given ResponseCodec[RawSuccessResponseText] =
    ResponseCodec
      .Raw(ExpectedStatuses.StatusRange.Success)
      .transformZIO(
        (status, headers, body) => body.asString.orDie.map(RawSuccessResponseText(status, headers, _, body.mediaType)),
        r => (r.status, r.headers, Body.fromString(r.body).optMediaType(r.contentType)),
      )
}

final case class RawErrorResponseText(
    status: Status,
    headers: Headers,
    body: String,
    contentType: Option[MediaType],
)
object RawErrorResponseText {
  given ResponseCodec[RawErrorResponseText] =
    ResponseCodec
      .Raw(ExpectedStatuses.StatusRange.Error)
      .transformZIO(
        (status, headers, body) => body.asString.orDie.map(RawErrorResponseText(status, headers, _, body.mediaType)),
        r => (r.status, r.headers, Body.fromString(r.body).optMediaType(r.contentType)),
      )
}
