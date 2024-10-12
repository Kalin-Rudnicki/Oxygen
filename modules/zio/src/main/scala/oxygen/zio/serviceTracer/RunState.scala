package oxygen.zio.serviceTracer

import java.util.UUID
import oxygen.core.syntax.option.*
import zio.*

sealed trait RunState
object RunState {

  sealed trait NotDisabled extends RunState {

    final def toTracing: UIO[RunState.Tracing] =
      Random.nextUUID.map { newId =>
        this match
          case Enabled           => Tracing(TraceIds(newId, newId, None))
          case Tracing(traceIds) => Tracing(TraceIds(newId, traceIds.rootId, traceIds.id.some))
      }

  }

  case object Disabled extends RunState // Service Tracing is disable
  case object Enabled extends NotDisabled // Service Tracing is enabled, but nothing would be done with any traces
  final case class Tracing(traceIds: TraceIds) extends NotDisabled // Service Tracing is enabled, and there is something to do with the traces

}
