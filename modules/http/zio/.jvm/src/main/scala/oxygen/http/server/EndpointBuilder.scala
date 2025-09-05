package oxygen.http.server

import zio.*

final class EndpointBuilder[-R: HasNoScope, +E] private (make: ZIO[R & Scope, E, Endpoints]) {

  private def toLayer: ZLayer[R, E, Endpoints] = ZLayer.scoped { make }

  def ++[R2 <: R: HasNoScope, E2 >: E, A: {Tag, DeriveEndpoints as de}](addLayer: ZLayer[R2, E2, A]): EndpointBuilder[R2, E2] =
    EndpointBuilder[R2, E2] {
      for {
        a <- make
        b <- addLayer.build.map { env => de.endpoints(env.get[A]) }
      } yield a ++ b
    }

}
object EndpointBuilder {

  def layer[R, E](f: EndpointBuilder[Any, Nothing] => EndpointBuilder[R, E]): ZLayer[R, E, Endpoints] = f(empty).toLayer

  private val empty: EndpointBuilder[Any, Nothing] = EndpointBuilder(ZIO.succeed(Endpoints.empty))

}
