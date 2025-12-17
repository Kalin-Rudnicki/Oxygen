package oxygen.http.server

import oxygen.http.model.internal.*
import zio.*
import zio.http.Path

trait RequestMiddleware {

  def apply(request: ReceivedRequest): URIO[Scope, ReceivedRequest]

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

  final case class AddPrefix(path: Path) extends RequestMiddleware.Effectless {
    override def applyEffectless(request: ReceivedRequest): ReceivedRequest = {
      val newPath: Path = path ++ request.url.path
      request.copy(url = request.url.copy(path = newPath))
    }
  }

  final case class DropPrefix(path: Path) extends RequestMiddleware.Effectless {
    override def applyEffectless(request: ReceivedRequest): ReceivedRequest = {
      val newPath: Path = request.url.path.unnest(path)
      request.copy(url = request.url.copy(path = newPath))
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Effectless extends RequestMiddleware {
    def applyEffectless(request: ReceivedRequest): ReceivedRequest
    override final def apply(request: ReceivedRequest): UIO[ReceivedRequest] = ZIO.succeed(applyEffectless(request))
  }

  case object Empty extends RequestMiddleware.Effectless {
    override def applyEffectless(request: ReceivedRequest): ReceivedRequest = request
  }

  final case class ThenEffectless(a: RequestMiddleware.Effectless, b: RequestMiddleware.Effectless) extends RequestMiddleware.Effectless {
    override def applyEffectless(request: ReceivedRequest): ReceivedRequest = b.applyEffectless(a.applyEffectless(request))
  }

  final case class ThenEffectful(a: RequestMiddleware, b: RequestMiddleware) extends RequestMiddleware {
    override def apply(request: ReceivedRequest): URIO[Scope, ReceivedRequest] = a.apply(request).flatMap(b.apply)
  }

}
