package oxygen.http.server

import oxygen.http.client.*
import scala.annotation.experimental
import zio.*

@experimental
final class EndpointAndClientBuilder[-R: HasNoScope, +E, A] private (
    private val endpoints: ZIO[R & Scope, E, AppliedEndpoints],
    private val client: URLayer[Client, A],
) {

  private def toLayer(using EnvironmentTag[A]): ZLayer[R & Client, E, AppliedEndpoints & A] = ZLayer.scoped { endpoints } ++ client

  def add[A2: {Tag, DeriveEndpoints as de, DeriveClient as dc}]: EndpointAndClientBuilder.PartiallyApplied[R, E, A, A2] =
    EndpointAndClientBuilder.PartiallyApplied(this)

}
object EndpointAndClientBuilder {

  final class PartiallyApplied[-R, +E, A, A2: {Tag, DeriveEndpoints as de, DeriveClient as dc}](builder: EndpointAndClientBuilder[R, E, A]) {

    def apply[R2 <: R: HasNoScope, E2 >: E](addLayer: ZLayer[R2, E2, A2]): EndpointAndClientBuilder[R2, E2, A & A2] =
      EndpointAndClientBuilder[R2, E2, A & A2](
        for {
          a <- builder.endpoints
          b <- addLayer.build.map { env => de.appliedEndpoints(env.get[A2]) }
        } yield a ++ b,
        builder.client ++ DeriveClient.clientLayer[A2],
      )

  }

  def layer[R, E, A: EnvironmentTag](f: EndpointAndClientBuilder[Any, Nothing, Any] => EndpointAndClientBuilder[R, E, A]): ZLayer[R & Client, E, AppliedEndpoints & A] = f(empty).toLayer

  private val empty: EndpointAndClientBuilder[Any, Nothing, Any] = EndpointAndClientBuilder(ZIO.succeed(AppliedEndpoints.empty), ZLayer.empty)

}
