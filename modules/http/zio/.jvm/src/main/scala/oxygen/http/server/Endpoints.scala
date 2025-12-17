package oxygen.http.server

import oxygen.predef.core.*
import zio.*

final class Endpoints[-Apis] private[server] (
    private val endpoints: Growable[Endpoints.Tagged[? >: Apis]],
) {

  def add[Api](using tag: Tag[Api], deriveEndpoints: DeriveEndpoints[Api]): Endpoints[Apis & Api] = {
    val newEndpoints: Growable[Endpoints.Tagged[Api]] =
      deriveEndpoints.endpoints.map(Endpoints.Tagged(tag, _))

    Endpoints[Apis & Api](this.endpoints ++ newEndpoints)
  }

  inline def deriveAndAdd[Api: Tag as tag]: Endpoints[Apis & Api] =
    this.add[Api](using tag, DeriveEndpoints.derived[Api])

  def ++[Apis2](that: Endpoints[Apis2]): Endpoints[Apis & Apis2] =
    Endpoints { this.endpoints ++ that.endpoints }

  def schemas: Growable[EndpointSchema] =
    this.endpoints.map(_.endpoint.schema)

  // TODO (KR) : compile schemas, but dont put that here, put it somewhere else shared

  def toLayer: URLayer[Apis, AppliedEndpoints] =
    ZLayer.fromZIO {
      ZIO.environment[Apis].map { apiEnv =>
        AppliedEndpoints(this.endpoints.map(_(apiEnv)))
      }
    }

}
object Endpoints {

  val empty: Endpoints[Any] = Endpoints[Any](Growable.empty)

  def flatten[Apis](
      endpoints: Endpoints[? >: Apis]*,
  ): Endpoints[Apis] =
    Endpoints { Growable.many(endpoints).flatMap(_.endpoints) }

  final case class Tagged[Api](
      tag: Tag[Api],
      endpoint: Endpoint[Api],
  ) {

    def apply(env: ZEnvironment[? <: Api]): AppliedEndpoint =
      endpoint.apply(env.get[Api](using tag))

  }

}
