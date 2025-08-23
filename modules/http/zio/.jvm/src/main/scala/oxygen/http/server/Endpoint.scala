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
    // TODO (KR) : other information about the endpoint
    handle: EndpointInput => Option[URIO[Scope, Response]],
) {

  val fullName: String = apiName match
    case Some(apiName) => s"$apiName.$endpointName"
    case None          => endpointName

  val metricLabels: Set[MetricLabel] =
    Set(
      apiName.map(MetricLabel("oxygen.api-name", _)),
      MetricLabel("oxygen.endpoint-name", endpointName).some,
    ).flatten

}
