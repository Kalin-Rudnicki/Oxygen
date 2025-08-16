package oxygen.http.server

import oxygen.zio.metrics.*
import zio.*
import zio.metrics.*

object HttpServerMetrics {

  val endpointDuration: Metric.Histogram[Duration] =
    MetricBuilders.microTimer(
      "oxygen.http.server.endpoint.duration",
      "How long it takes the endpoint to handle the request.",
      50.micros,
      1.hour,
    )

  val requestDuration: Metric.Histogram[Duration] =
    MetricBuilders.microTimer(
      "oxygen.http.server.request.duration",
      "How long it takes the Oxygen HTTP Server to handle the HTTP request.",
      50.micros,
      1.hour,
    )

}
