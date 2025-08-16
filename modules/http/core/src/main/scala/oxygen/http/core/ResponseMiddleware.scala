package oxygen.http.core

import oxygen.http.model.*
import oxygen.zio.syntax.log.*
import oxygen.zio.tracing.*
import zio.*

trait ResponseMiddleware {
  def map(response: HttpResponse): URIO[Scope, HttpResponse]
}
object ResponseMiddleware {

  val sendOxygenTracing: ResponseMiddleware =
    response =>
      TraceState.current.get.map {
        case Some(value) => response.addHeaders(Headers.of(TraceState.currentHeader -> value.current.toString))
        case None        => response
      }

  val receiveOxygenTracing: ResponseMiddleware =
    response =>
      response.singleHeader(TraceState.currentHeader) match {
        case Some(value) => ZIO.logInfoAnnotated("Received trace-id from server", s"server:${TraceState.currentHeader}" -> value).as(response)
        case None        => ZIO.succeed(response)
      }

}
