package oxygen.zio.tracing

import java.util.UUID
import oxygen.predef.core.*
import oxygen.zio.*
import zio.*

enum TraceState {
  case Root(rootId: UUID)
  case NonRoot(rootId: UUID, parentId: UUID, currentId: UUID)

  final def rootAndNewParent: (UUID, UUID) = this match
    case TraceState.Root(rootId)                  => (rootId, rootId)
    case TraceState.NonRoot(rootId, _, currentId) => (rootId, currentId)

}
object TraceState {

  val current: FiberRef[Option[TraceState]] = Unsafe.unsafely { FiberRef.unsafe.make(None) }

  val rootHeader: String = "X-Oxygen-Trace-Root"
  val parentHeader: String = "X-Oxygen-Trace-Parent"
  val currentHeader: String = "X-Oxygen-Trace-Id"

  def getOrSetRoot: URIO[Scope, TraceState] =
    current.getWith {
      case Some(value) => ZIO.succeed(value)
      case None        =>
        for {
          rootId <- Random.nextUUID
          value = Root(rootId)
          rootIdString = rootId.toString
          _ <- ZIO.logAnnotateScoped(LogAnnotation(rootHeader, rootIdString), LogAnnotation(currentHeader, rootIdString))
          _ <- current.locallyScoped(value.some)
        } yield value
    }

  def setRootAndParent(rootId: UUID, parentId: UUID): URIO[Scope, TraceState] =
    current.getWith { c =>
      for {
        _ <- ZIO.logWarning("Overriding trace state").whenDiscard(c.nonEmpty)
        currentId <- Random.nextUUID
        value = NonRoot(rootId, parentId, currentId)
        _ <- ZIO.logAnnotateScoped(LogAnnotation(rootHeader, rootId.toString), LogAnnotation(parentHeader, parentId.toString), LogAnnotation(currentHeader, currentId.toString))
        _ <- current.locallyScoped(value.some)
      } yield value
    }

}
