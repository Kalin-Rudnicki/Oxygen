package zio.compat

import zio.Unsafe
import zio.internal.metrics.metricRegistry
import zio.metrics.MetricPair

object metricOps {

  def snapshot(): Set[MetricPair.Untyped] =
    Unsafe.unsafely { metricRegistry.snapshot() }

}
