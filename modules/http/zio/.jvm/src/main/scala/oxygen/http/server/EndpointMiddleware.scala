package oxygen.http.server

import zio.*

trait EndpointMiddleware {

  def apply(endpoints: AppliedEndpoints): URIO[Scope, AppliedEndpoints]

  final def >>>(that: EndpointMiddleware): EndpointMiddleware =
    (this, that) match
      case (EndpointMiddleware.Empty, _) => that
      case (_, EndpointMiddleware.Empty) => this
      case _                             => this.apply(_).flatMap(that.apply)

}
object EndpointMiddleware {

  case object Empty extends EndpointMiddleware {
    override def apply(endpoints: AppliedEndpoints): UIO[AppliedEndpoints] = ZIO.succeed(endpoints)
  }

  // TODO (KR) : docs

}
