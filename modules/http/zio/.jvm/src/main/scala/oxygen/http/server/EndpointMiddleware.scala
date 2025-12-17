package oxygen.http.server

import zio.*

trait EndpointMiddleware {

  def apply(endpoints: AppliedEndpoints): URIO[Scope, AppliedEndpoints]

  final def >>>(that: EndpointMiddleware): EndpointMiddleware =
    (this, that) match
      case (EndpointMiddleware.Empty, _)                                              => that
      case (_, EndpointMiddleware.Empty)                                              => this
      case (self: EndpointMiddleware.Effectless, that: EndpointMiddleware.Effectless) => EndpointMiddleware.ThenEffectless(self, that)
      case _                                                                          => EndpointMiddleware.ThenEffectful(this, that)

}
object EndpointMiddleware {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Default Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) :

  // TODO (KR) : docs

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Effectless extends EndpointMiddleware {
    def applyEffectless(endpoints: AppliedEndpoints): AppliedEndpoints
    override final def apply(endpoints: AppliedEndpoints): URIO[Scope, AppliedEndpoints] = ZIO.succeed(endpoints)
  }

  case object Empty extends EndpointMiddleware.Effectless {
    override def applyEffectless(endpoints: AppliedEndpoints): AppliedEndpoints = endpoints
  }

  final case class ThenEffectless(a: EndpointMiddleware.Effectless, b: EndpointMiddleware.Effectless) extends EndpointMiddleware.Effectless {
    override def applyEffectless(endpoints: AppliedEndpoints): AppliedEndpoints = b.applyEffectless(a.applyEffectless(endpoints))
  }

  final case class ThenEffectful(a: EndpointMiddleware, b: EndpointMiddleware) extends EndpointMiddleware {
    override def apply(endpoints: AppliedEndpoints): URIO[Scope, AppliedEndpoints] = a.apply(endpoints).flatMap(b.apply)
  }

}
