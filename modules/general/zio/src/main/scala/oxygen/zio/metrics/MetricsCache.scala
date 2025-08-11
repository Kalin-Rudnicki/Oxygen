package oxygen.zio.metrics

import java.time.Instant
import oxygen.predef.core.*
import zio.*
import zio.compat.metricOps

final class MetricsCache private (cache: Ref[MetricsCache.CacheState]) {

  val read: UIO[MetricsCache.CacheState] = cache.get

}
object MetricsCache {

  final case class CacheState(lastUpdatedAt: Option[Instant], metrics: GroupedMetrics)

  private def make(initial: => CacheState): UIO[MetricsCache] =
    Ref.make { initial }.map(MetricsCache(_))

  val makeEmpty: UIO[MetricsCache] = make { CacheState(None, GroupedMetrics(Map.empty)) }

  val makeCurrent: UIO[MetricsCache] = make { CacheState(Instant.now().some, GroupedMetrics.group(metricOps.snapshot())) }

}
