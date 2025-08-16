package oxygen.sql

import oxygen.zio.metrics.MetricBuilders
import zio.*
import zio.metrics.*

object SqlMetrics {

  val queryDuration: Metric.Histogram[Duration] =
    MetricBuilders.microTimer(
      "oxygen.sql.query.duration",
      10.micros,
      1.hour,
    )

}
