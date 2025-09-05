package oxygen.http.server

import oxygen.predef.core.*
import zio.*

final case class Endpoints(endpoints: Growable[Endpoint]) {

  lazy val arraySeq: ArraySeq[Endpoint] = endpoints.toArraySeq
  def ++(that: Endpoints): Endpoints = Endpoints(this.endpoints ++ that.endpoints)

  def compile(
      requestMiddleware: RequestMiddleware,
      responseMiddleware: ResponseMiddleware,
      endpointMiddleware: EndpointMiddleware,
  ): URIO[Scope, CompiledEndpoints] =
    endpointMiddleware(this).map { finalEndpoints =>
      CompiledEndpoints.SeqScan(finalEndpoints).withMiddleware(requestMiddleware, responseMiddleware)
    }

}
object Endpoints {

  def flatten(endpoints: Endpoints*): Endpoints =
    Endpoints(Growable.many(endpoints).flatMap(_.endpoints))

  val empty: Endpoints = Endpoints(Growable.empty)

  def layer[R, E](f: EndpointBuilder[Any, Nothing] => EndpointBuilder[R, E]): ZLayer[R, E, Endpoints] = EndpointBuilder.layer(f)

}
