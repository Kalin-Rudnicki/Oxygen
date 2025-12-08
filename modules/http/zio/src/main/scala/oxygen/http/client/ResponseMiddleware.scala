package oxygen.http.client

import oxygen.http.model.internal.ReceivedResponse
import zio.*

trait ResponseMiddleware {

  def apply(response: ReceivedResponse): URIO[Scope, ReceivedResponse]

  final def >>>(that: ResponseMiddleware): ResponseMiddleware =
    (this, that) match
      case (ResponseMiddleware.Empty, _) => that
      case (_, ResponseMiddleware.Empty) => this
      case _                             => this.apply(_).flatMap(that.apply)

}
object ResponseMiddleware {

  case object Empty extends ResponseMiddleware {
    override def apply(response: ReceivedResponse): UIO[ReceivedResponse] = ZIO.succeed(response)
  }

}
