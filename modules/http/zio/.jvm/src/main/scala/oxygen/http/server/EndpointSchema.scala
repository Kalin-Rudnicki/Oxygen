package oxygen.http.server

import oxygen.http.schema.*
import oxygen.predef.core.*
import zio.metrics.MetricLabel

final case class EndpointSchema(
    apiName: Option[String],
    endpointName: String,
    requestSchema: RequestSchema,
    successResponseSchema: ResponseSchema,
    errorResponseSchema: ResponseSchema,
    doc: Option[String],
) {

  val metricLabels: Set[MetricLabel] =
    Set(
      apiName.map(MetricLabel("oxygen.api-name", _)),
      MetricLabel("oxygen.endpoint-name", endpointName).some,
    ).flatten

}
