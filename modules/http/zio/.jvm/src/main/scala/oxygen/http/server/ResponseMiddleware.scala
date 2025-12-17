package oxygen.http.server

import zio.*
import zio.http.*

trait ResponseMiddleware {

  def apply(response: Response): URIO[Scope, Response]

  final def >>>(that: ResponseMiddleware): ResponseMiddleware =
    (this, that) match
      case (ResponseMiddleware.Empty, _)                                              => that
      case (_, ResponseMiddleware.Empty)                                              => this
      case (self: ResponseMiddleware.Effectless, that: ResponseMiddleware.Effectless) => ResponseMiddleware.ThenEffectless(self, that)
      case _                                                                          => ResponseMiddleware.ThenEffectful(this, that)

}
object ResponseMiddleware {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Default Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) :

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Effectless extends ResponseMiddleware {
    def applyEffectless(response: Response): Response
    override final def apply(response: Response): URIO[Scope, Response] = ZIO.succeed(response)
  }

  case object Empty extends ResponseMiddleware.Effectless {
    override def applyEffectless(response: Response): Response = response
  }

  final case class ThenEffectless(a: ResponseMiddleware.Effectless, b: ResponseMiddleware.Effectless) extends ResponseMiddleware.Effectless {
    override def applyEffectless(response: Response): Response = b.applyEffectless(a.applyEffectless(response))
  }

  final case class ThenEffectful(a: ResponseMiddleware, b: ResponseMiddleware) extends ResponseMiddleware {
    override def apply(response: Response): URIO[Scope, Response] = a.apply(response).flatMap(b.apply)
  }

}
