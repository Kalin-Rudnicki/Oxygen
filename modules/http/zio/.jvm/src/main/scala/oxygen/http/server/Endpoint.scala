package oxygen.http.server

import oxygen.http.schema.*
import oxygen.predef.core.*
import zio.*
import zio.http.*
import zio.metrics.MetricLabel

final case class Endpoint(
    apiName: Option[String],
    endpointName: String,
    requestSchema: RequestSchema,
    successResponseSchema: ResponseSchema,
    errorResponseSchema: ResponseSchema,
    doc: Option[String],
    // TODO (KR) : other information about the endpoint? (wtf was I talking about here...)
    handle: EndpointInput => Option[URIO[Scope, Option[Response]]],
) {

  val fullName: String = apiName match
    case Some(apiName) => s"$apiName.$endpointName"
    case None          => endpointName

  // TODO (KR) : use these
  val metricLabels: Set[MetricLabel] =
    Set(
      apiName.map(MetricLabel("oxygen.api-name", _)),
      MetricLabel("oxygen.endpoint-name", endpointName).some,
    ).flatten

}
