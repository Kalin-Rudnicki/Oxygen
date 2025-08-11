package oxygen.zio.metrics

final case class MetricLabels(labels: Map[String, String]) extends CalculatedHash
