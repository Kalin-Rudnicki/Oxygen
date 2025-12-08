package oxygen.http.server

import oxygen.http.model.internal.*
import zio.*

trait RequestMiddleware {

  def apply(request: ReceivedRequest): URIO[Scope, ReceivedRequest]

  final def >>>(that: RequestMiddleware): RequestMiddleware =
    (this, that) match
      case (RequestMiddleware.Empty, _) => that
      case (_, RequestMiddleware.Empty) => this
      case _                            => this.apply(_).flatMap(that.apply)

}
object RequestMiddleware {

  case object Empty extends RequestMiddleware {
    override def apply(request: ReceivedRequest): UIO[ReceivedRequest] = ZIO.succeed(request)
  }

}
