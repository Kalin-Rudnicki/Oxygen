package oxygen.http.server

import oxygen.http.core.generic.*
import oxygen.http.server.generic.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

trait DeriveEndpoints[-Api] {
  def endpoints: Growable[Endpoint[Api]]
  final def appliedEndpoints(api: Api): AppliedEndpoints = AppliedEndpoints { endpoints.map(_(api)) }
}
object DeriveEndpoints {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[server] def derivedImpl[Api: Type](using Quotes): Expr[DeriveEndpoints[Api]] = {
    val api: ApiRepr[Api] = ApiRepr.derive[Api]

    val endpoints: ArraySeq[EndpointRepr[Api]] =
      api.routes.map(EndpointRepr[Api](_))

    def endpointsImpl(
        queue: List[EndpointRepr[Api]],
        rStack: List[EndpointRepr.WithImpl[Api]],
    )(using Quotes): Expr[DeriveEndpoints[Api]] =
      queue match {
        case head :: tail =>
          head.withImpl[DeriveEndpoints[Api]] { impl => endpointsImpl(tail, impl :: rStack) }
        case Nil =>
          '{
            new DeriveEndpoints[Api] {
              override def endpoints: Growable[Endpoint[Api]] =
                ${ Growable.many(rStack.reverse).map(_.toEndpoint).seqToExpr }
            }
          }
      }

    // TODO (KR) : Use a different `Symbol.newClass` builder which allows creating a primary constructor.
    //           : Then, be able to do `Block(List(newClassDef), '{ new DeriveClient[Api} { ... } })`,
    //           : instead of needing to define the class in the `def client`.
    val deriveEndpointsExpr: Expr[DeriveEndpoints[Api]] =
      endpointsImpl(endpoints.toList, Nil)

    // import oxygen.quoted.*
    // report.errorAndAbort(deriveEndpointsExpr.showAnsiCode)

    deriveEndpointsExpr
  }

  inline def derived[A]: DeriveEndpoints[A] = ${ derivedImpl[A] }

}
