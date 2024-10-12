package oxygen.zio.serviceTracer

import java.time.Instant
import java.util.UUID
import oxygen.zio.Outcome
import zio.*
import zio.json.JsonCodec

sealed trait TraceElem derives JsonCodec
object TraceElem {

  final case class Start(
      traceIds: TraceIds,
      closure: TraceClosure,
      threadName: String,
      start: Instant,
  ) extends TraceElem

  final case class End(
      id: UUID,
      end: Instant,
      outcome: Outcome,
  ) extends TraceElem

}
