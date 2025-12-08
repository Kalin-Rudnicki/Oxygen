package oxygen.http.schema

import oxygen.predef.core.*
import zio.http.Method

final case class RequestSchema private[http] (
    method: Option[Method],
    paths: NonEmptyList[RequestPathsSchema],
    queryParams: ArraySeq[RequestQueryParamSchema],
    headers: ArraySeq[RequestHeaderSchema],
    body: RequestBodySchema,
)
