package oxygen.http.schema

import oxygen.predef.core.*

// TODO (KR) : this needs lots of additional details
final case class ResponseSchema(
    expectedStatuses: ExpectedStatuses,
    headers: ArraySeq[ResponseHeaderSchema],
    body: ResponseBodySchema,
)
