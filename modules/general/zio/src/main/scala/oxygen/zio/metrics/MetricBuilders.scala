package oxygen.zio.metrics

import java.time.temporal.ChronoUnit
import zio.*
import zio.metrics.*

object MetricBuilders {

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

  def microTimer(
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

  def microTimer(
      name: String,
      description: String,
      min: Duration,
      max: Duration,
  ): Metric.Histogram[Duration] =
    Metric.timer(
      name,
      description,
      ChronoUnit.MICROS,
      microDurations.filter { num =>
        val dur = (num * 1000).toLong.nanos
        dur >= min && dur <= max
      },
    )

  def effectResult(exit: Exit[?, ?]): String =
    exit match {
      case Exit.Success(_)     => "success"
      case Exit.Failure(cause) =>
        if (cause.isFailure) "failure"
        else if (cause.isDie) "defect"
        else if (cause.isInterrupted) "interrupt"
        else "unknown"
    }

}
