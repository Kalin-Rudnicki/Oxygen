package oxygen.http.client

import oxygen.zio.metrics.*
import zio.*
import zio.metrics.*

object HttpClientMetrics {

  val endpointDuration: Metric.Histogram[Duration] =
    MetricBuilders.microTimer(
      "oxygen.http.client.endpoint.duration",
      "How long it takes to encode, send, receive, and parse the HTTP response.",
      50.micros,
      1.hour,
    )

  val requestDuration: Metric.Histogram[Duration] =
    MetricBuilders.microTimer(
      "oxygen.http.client.request.duration",
      "How long it takes the send and receive the HTTP request.",
      50.micros,
      1.hour,
    )

}
