package oxygen.http.schema

import oxygen.predef.core.*
import zio.http.Status

final case class ResponseSchema(
    expectedStatuses: ExpectedStatuses,
    headers: ArraySeq[ResponseHeaderSchema],
    body: ResponseBodySchema,
    // for sum-typed responses: the status each case maps to, keyed by case name (matches the body's sum-case names)
    caseStatuses: ArraySeq[(String, Status)] = ArraySeq.empty,
)
