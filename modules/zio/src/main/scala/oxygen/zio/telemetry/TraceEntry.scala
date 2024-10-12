package oxygen.zio.telemetry

import java.time.Instant
import oxygen.zio.Outcome
import zio.Duration
import zio.json.JsonCodec

final case class TraceEntry(
    key: String,
    args: Map[String, String],
    start: Instant,
    end: Instant,
    outcome: Outcome,
) derives JsonCodec {
  lazy val duration: Duration = Duration.fromInterval(start, end)
}
