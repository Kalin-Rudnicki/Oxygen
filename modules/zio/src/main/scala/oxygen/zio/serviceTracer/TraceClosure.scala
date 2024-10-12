package oxygen.zio.serviceTracer

import zio.json.JsonCodec

final case class TraceClosure(
    serviceName: String,
    serviceImpl: String,
    function: String,
    params: Map[String, String],
) derives JsonCodec {
  override def toString: String = s"$serviceName[$serviceImpl].$function(${params.map { case (k, v) => s"$k = $v" }.mkString(", ")})"
}
