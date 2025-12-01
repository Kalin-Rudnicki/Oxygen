package oxygen.zio.metrics

import oxygen.zio.*
import zio.*
import zio.metrics.*

extension (self: Metric.Histogram[Duration])
  def toAspect: ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(using trace: Trace): ZIO[R, E, A] =
        effect.exit.timed.flatMap { case (duration, exit) =>
          self.tagged("oxygen.effect-result", MetricBuilders.effectResult(exit)).update(duration) *>
            exit
        }
    }
