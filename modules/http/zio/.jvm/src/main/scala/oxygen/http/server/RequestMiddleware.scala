package oxygen.http.server

import zio.*
import zio.http.*

trait RequestMiddleware {

  def apply(request: Request): URIO[Scope, Request]

  final def >>>(that: RequestMiddleware): RequestMiddleware =
    (this, that) match
      case (RequestMiddleware.Empty, _) => that
      case (_, RequestMiddleware.Empty) => this
      case _                            => this.apply(_).flatMap(that.apply)

}
object RequestMiddleware {

  case object Empty extends RequestMiddleware {
    override def apply(request: Request): UIO[Request] = ZIO.succeed(request)
  }

}
