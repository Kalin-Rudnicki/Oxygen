package oxygen.http.core

import java.util.UUID
import oxygen.http.model.*
import oxygen.zio.tracing.*
import scala.util.Try
import zio.*

trait RequestMiddleware {
  def map(request: HttpRequest): URIO[Scope, HttpRequest]
}
object RequestMiddleware {

  val sendOxygenTracing: RequestMiddleware =
    request =>
      TraceState.getOrSetRoot.map { traceState =>
        val (newRoot, newParent) = traceState.rootAndNewParent
        request.addHeaders(Headers.of((TraceState.rootHeader, newRoot.toString), (TraceState.parentHeader, newParent.toString)))
      }

  private def parseTraceIds(request: HttpRequest): Option[(UUID, UUID)] = {
    def header(h: String): Option[UUID] =
      request.header(h).headOption.flatMap { v => Try { UUID.fromString(v) }.toOption }

    for {
      root <- header(TraceState.rootHeader)
      parent <- header(TraceState.parentHeader)
    } yield (root, parent)
  }

  val receiveOxygenTracing: RequestMiddleware =
    request =>
      parseTraceIds(request) match {
        case Some((rootId, parentId)) => TraceState.setRootAndParent(rootId, parentId).as(request)
        case None                     => TraceState.getOrSetRoot.as(request)
      }

}
