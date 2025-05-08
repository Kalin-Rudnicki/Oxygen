package oxygen.zio.logging

import oxygen.json.JsonCodec
import oxygen.zio.instances.given

final case class EncodedLogEvent(
    logLevel: zio.LogLevel,
    message: String,
    trace: zio.Trace,
    fiberId: zio.FiberId,
    annotations: Map[String, String],
    spans: Map[String, Long],
    cause: Option[EncodedLogCause],
) derives JsonCodec
