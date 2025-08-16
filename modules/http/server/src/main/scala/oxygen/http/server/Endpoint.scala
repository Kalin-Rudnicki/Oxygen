package oxygen.http.server

import oxygen.core.syntax.option.*
import oxygen.http.core.PathCodec
import oxygen.http.model.*
import zio.*
import zio.metrics.MetricLabel

final case class Endpoint(
    apiName: Option[String],
    endpointName: String,
    pathElems: List[List[PathCodec.Spec]],
    run: RequestContext => Option[URIO[Scope, HttpResponse]],
) {
  val fullName: String = apiName.fold(endpointName)(n => s"$n.$endpointName")
  val metricLabels: Set[MetricLabel] =
    Set(
      apiName.map(MetricLabel("oxygen.api-name", _)),
      MetricLabel("oxygen.endpoint-name", endpointName).some,
    ).flatten
}
