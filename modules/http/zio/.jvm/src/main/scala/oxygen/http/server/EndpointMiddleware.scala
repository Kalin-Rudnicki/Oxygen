package oxygen.http.server

import zio.*

trait EndpointMiddleware {

  def apply(endpoints: Endpoints): URIO[Scope, Endpoints]

  final def >>>(that: EndpointMiddleware): EndpointMiddleware =
    (this, that) match
      case (EndpointMiddleware.Empty, _) => that
      case (_, EndpointMiddleware.Empty) => this
      case _                             => this.apply(_).flatMap(that.apply)

}
object EndpointMiddleware {

  case object Empty extends EndpointMiddleware {
    override def apply(endpoints: Endpoints): UIO[Endpoints] = ZIO.succeed(endpoints)
  }

}
