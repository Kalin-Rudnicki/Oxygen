package oxygen.zio.metrics

import zio.metrics.MetricKeyType

final case class NamedKey(
    name: String,
    keyType: MetricKeyType,
) extends CalculatedHash
