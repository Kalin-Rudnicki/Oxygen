package oxygen.http.client

import zio.*
import zio.http.*

trait RequestMiddleware {

  def apply(request: Request): URIO[Scope, Request]

  final def >>>(that: RequestMiddleware): RequestMiddleware =
    (this, that) match
      case (RequestMiddleware.Empty, _)                                             => that
      case (_, RequestMiddleware.Empty)                                             => this
      case (self: RequestMiddleware.Effectless, that: RequestMiddleware.Effectless) => RequestMiddleware.ThenEffectless(self, that)
      case _                                                                        => RequestMiddleware.ThenEffectful(this, that)

}
object RequestMiddleware {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Default Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) :

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Effectless extends RequestMiddleware {
    def applyEffectless(request: Request): Request
    override final def apply(request: Request): UIO[Request] = ZIO.succeed(applyEffectless(request))
  }

  case object Empty extends RequestMiddleware.Effectless {
    override def applyEffectless(request: Request): Request = request
  }

  final case class ThenEffectless(a: RequestMiddleware.Effectless, b: RequestMiddleware.Effectless) extends RequestMiddleware.Effectless {
    override def applyEffectless(request: Request): Request = b.applyEffectless(a.applyEffectless(request))
  }

  final case class ThenEffectful(a: RequestMiddleware, b: RequestMiddleware) extends RequestMiddleware {
    override def apply(request: Request): URIO[Scope, Request] = a.apply(request).flatMap(b.apply)
  }

}
