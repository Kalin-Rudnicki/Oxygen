package oxygen.http.client

import oxygen.http.model.internal.ReceivedResponse
import zio.*

trait ResponseMiddleware {

  def apply(response: ReceivedResponse): URIO[Scope, ReceivedResponse]

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
    def applyEffectless(response: ReceivedResponse): ReceivedResponse
    override final def apply(response: ReceivedResponse): URIO[Scope, ReceivedResponse] = ZIO.succeed(response)
  }

  case object Empty extends ResponseMiddleware.Effectless {
    override def applyEffectless(response: ReceivedResponse): ReceivedResponse = response
  }

  final case class ThenEffectless(a: ResponseMiddleware.Effectless, b: ResponseMiddleware.Effectless) extends ResponseMiddleware.Effectless {
    override def applyEffectless(response: ReceivedResponse): ReceivedResponse = b.applyEffectless(a.applyEffectless(response))
  }

  final case class ThenEffectful(a: ResponseMiddleware, b: ResponseMiddleware) extends ResponseMiddleware {
    override def apply(response: ReceivedResponse): URIO[Scope, ReceivedResponse] = a.apply(response).flatMap(b.apply)
  }

}
