package oxygen.sql

import java.time.temporal.ChronoUnit
import zio.*
import zio.metrics.*

object SqlMetrics {

  private val incs10: Chunk[Double] = Chunk(1.0, 2.5, 5.0)
  private val incs60: Chunk[Int] = Chunk(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 40, 50)

  private val sub1s: Chunk[Double] =
    Chunk(
      incs10.map { _ * 1.0 },
      incs10.map { _ * 10.0 },
      incs10.map { _ * 100.0 },
      incs10.map { _ * 1_000.0 },
      incs10.map { _ * 10_000.0 },
      incs10.map { _ * 100_000.0 },
    ).flatten

  private val _1s: Double = 1_000_000.0
  private val _1m: Double = _1s * 60
  private val _1h: Double = _1m * 60

  // TODO (KR) : move to somewhere shared
  private val microDurations: Chunk[Double] =
    Chunk(
      sub1s,
      incs60.map { _ * _1s },
      incs60.map { _ * _1m },
      Chunk(1, 2, 3, 6, 12, 24).map { _ * _1h },
    ).flatten

  private def makeTimer(
      name: String,
      min: Duration,
      max: Duration,
  ): Metric.Histogram[Duration] =
    Metric.timer(
      name,
      ChronoUnit.MICROS,
      microDurations.filter { num =>
        val dur = (num * 1000).toLong.nanos
        dur >= min && dur <= max
      },
    )

  val queryDuration: Metric.Histogram[Duration] =
    makeTimer(
      "oxygen.sql.query.duration",
      10.micros,
      1.hour,
    )

}
