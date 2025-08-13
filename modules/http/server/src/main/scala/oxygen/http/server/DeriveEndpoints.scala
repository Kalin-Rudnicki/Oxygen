package oxygen.http.server

import oxygen.http.core.generic.*
import oxygen.http.server.generic.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

trait DeriveEndpoints[-Api] {
  def endpoints(api: Api): Endpoints
}
object DeriveEndpoints {

  def endpoints[Api: DeriveEndpoints as der](api: Api): Endpoints =
    der.endpoints(api)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[server] def derivedImpl[Api: Type](using Quotes): Expr[DeriveEndpoints[Api]] = {
    val api: ApiRepr[Api] = ApiRepr.derive[Api]

    val endpoints: ArraySeq[EndpointRepr[Api]] =
      api.routes.map(EndpointRepr[Api](_))

    def endpointsImpl(apiExpr: Expr[Api]): Expr[Growable[Endpoint]] =
      Growable.many(endpoints.map(_.toEndpoint(apiExpr))).seqToExpr

    '{
      new DeriveEndpoints[Api] {
        override def endpoints(api: Api): Endpoints = Endpoints(${ endpointsImpl('api) })
      }
    }
  }

  inline def derived[A]: DeriveEndpoints[A] = ${ derivedImpl[A] }

}
