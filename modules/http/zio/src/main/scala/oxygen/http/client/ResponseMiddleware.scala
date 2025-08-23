package oxygen.http.client

import zio.*
import zio.http.*

trait ResponseMiddleware {

  def apply(response: Response): URIO[Scope, Response]

  final def >>>(that: ResponseMiddleware): ResponseMiddleware =
    (this, that) match
      case (ResponseMiddleware.Empty, _) => that
      case (_, ResponseMiddleware.Empty) => this
      case _                             => this.apply(_).flatMap(that.apply)

}
object ResponseMiddleware {

  case object Empty extends ResponseMiddleware {
    override def apply(response: Response): UIO[Response] = ZIO.succeed(response)
  }

}
