package oxygen.zio.metrics

import oxygen.predef.core.*
import zio.Chunk
import zio.metrics.*

final case class GroupedMetrics(
    metrics: Map[NamedKey, Map[MetricLabels, MetricState.Untyped]],
)
object GroupedMetrics {

  def group(metrics: Set[MetricPair.Untyped]): GroupedMetrics = {
    val iter: Iterator[(NamedKey, (MetricLabels, MetricState.Untyped))] =
      metrics.iterator.map { m => (NamedKey(m.metricKey.name, m.metricKey.keyType), (MetricLabels(m.metricKey.tags.iterator.map { l => (l.key, l.value) }.toMap), m.metricState)) }

    GroupedMetrics(Chunk.from(iter).groupMap(_._1)(_._2).map { case (k, vs) => (k, vs.toMap) })
  }

  private def diffState(prev: MetricState.Untyped, current: MetricState.Untyped): MetricState.Untyped =
    current match {
      case (prev: MetricState.Counter, current: MetricState.Counter) =>
        MetricState.Counter(current - prev)
      case (prev: MetricState.Histogram, current: MetricState.Histogram) =>
        ???
      case (prev: MetricState.Frequency, current: MetricState.Frequency) =>
        MetricState.Frequency(
          Ior
            .zipMapIterator(prev.occurrences, current.occurrences)
            .flatMap {
              case (_, Ior.Both(left, right)) if left == right => None
              case (key, Ior.Both(left, right))                => (key, right - left).some
              case (_, Ior.Left(_))                            => None
              case (key, Ior.Right(right))                     => (key, right).some
            }
            .toMap,
        )
      case (_: MetricState.Gauge, current: MetricState.Gauge) =>
        current
      case (prev: MetricState.Summary, current: MetricState.Summary) => ???
      case _                                                         =>
        throw new RuntimeException(s"metric state prev/current are of a different type?\nprev: $prev\ncurrent:$current")
    }

  def diff(prev: GroupedMetrics, current: GroupedMetrics): GroupedMetrics = {
    Ior.zipMapIterator(prev.metrics, current.metrics).flatMap {
      case (namedKey, Ior.Both(left, right)) =>
        val newMap: Map[MetricLabels, MetricState.Untyped] =
          Ior
            .zipMapIterator(left, right)
            .flatMap {
              case (_, Ior.Both(left, right)) if left == right => None
              case (labels, Ior.Both(left, right))             => (labels, diffState(left, right)).some
              case (_, Ior.Left(_))                            => None
              case (labels, Ior.Right(right))                  => (labels, right).some
            }
            .toMap

        Option.when(newMap.nonEmpty) { (namedKey, newMap) }
      case (_, Ior.Left(_)) =>
        None
      case (namedKey, Ior.Right(right)) =>
        (namedKey, right).some
    }

    ???
  }

}
