package oxygen.http.server.generic

import oxygen.http.core.ServerSentEvents
import oxygen.http.core.generic.*
import oxygen.http.server.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

sealed trait EndpointRepr[Api] {

  val route: RouteRepr[Api]
  import route.given

  type In = route.In

  protected def makeImpl(using Quotes): Expr[DerivedServerEndpointImpl[Api, In]]

  final def withImpl[O: Type](f: EndpointRepr.WithImpl[Api] => Expr[O])(using Quotes): Expr[O] =
    ValDef.companion.letExpr[DerivedServerEndpointImpl[Api, In], O](
      route.defDef.name + "__Impl",
      makeImpl,
      ValDef.ValType.LazyVal,
    ) { expr => f(EndpointRepr.WithImpl(this)(expr)) }

  protected final def serverErrorHandlerExpr(using Quotes): Expr[ServerErrorHandler[route.ErrorOut]] =
    Implicits.searchOption[ServerErrorHandler[route.ErrorOut]].getOrElse { route.fail(s"No given instance found for: ${TypeRepr.of[ServerErrorHandler[route.ErrorOut]].showAnsiCode}") }

}
object EndpointRepr {

  final class WithImpl[Api](val endpointRepr: EndpointRepr[Api])(val impl: Expr[DerivedServerEndpointImpl[Api, endpointRepr.In]]) {
    import endpointRepr.route.given

    def toEndpoint(apiExpr: Expr[Api])(using Quotes): Expr[Endpoint] =
      '{ $impl.toEndpoint($apiExpr) }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromZIO
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class FromZIO[Api](val route: RouteRepr.ReturningZIO[Api]) extends EndpointRepr[Api] {
    import route.given

    override protected def makeImpl(using Quotes): Expr[DerivedServerEndpointImpl[Api, In]] =
      '{
        DerivedServerEndpointImpl.FromZIO[Api, route.In, route.ErrorOut, route.SuccessOut](
          apiName = ${ Expr(route.derivedApiName) },
          endpointName = ${ Expr(route.derivedEndpointName) },
          doc = ${ Expr(route.defDefDoc) },
          requestCodec = ${ route.requestCodec },
          errorResponseCodec = ${ route.errorResponseCodec },
          successResponseCodec = ${ route.successResponseCodec },
          serverErrorHandler = $serverErrorHandlerExpr,
          impl = { (api: Api, in: route.In) =>
            ${
              val apiTerm: Term = 'api.toTerm
              val inExpr: Expr[route.In] = 'in
              val paramTerms: List[Term] = route.inExprToParamOrderTerms(inExpr)
              val appliedTerm: Term = apiTerm.select(route.defDef.symbol).appliedToArgs(paramTerms)
              appliedTerm.asExprOf[ZIO[Scope, route.ErrorOut, route.SuccessOut]]
            }
          },
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromSSE
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class FromSSE[Api](val route: RouteRepr.ReturningSSE[Api]) extends EndpointRepr[Api] {
    import route.given

    override protected def makeImpl(using Quotes): Expr[DerivedServerEndpointImpl[Api, In]] =
      '{
        DerivedServerEndpointImpl.FromSSE[Api, route.In, route.ErrorOut, route.SuccessOut](
          apiName = ${ Expr(route.derivedApiName) },
          endpointName = ${ Expr(route.derivedEndpointName) },
          doc = ${ Expr(route.defDefDoc) },
          requestCodec = ${ route.requestCodec },
          errorResponseCodec = ${ route.errorResponseCodec },
          schema = ${ route.successSchema },
          serverErrorHandler = $serverErrorHandlerExpr,
          impl = { (api: Api, in: route.In) =>
            ${
              val apiTerm: Term = 'api.toTerm
              val inExpr: Expr[route.In] = 'in
              val paramTerms: List[Term] = route.inExprToParamOrderTerms(inExpr)
              val appliedTerm: Term = apiTerm.select(route.defDef.symbol).appliedToArgs(paramTerms)
              appliedTerm.asExprOf[ServerSentEvents[route.ErrorOut, route.SuccessOut]]
            }
          },
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Entry/Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply[Api](route: RouteRepr[Api]): EndpointRepr[Api] =
    route match
      case route: RouteRepr.ReturningZIO[Api] => EndpointRepr.FromZIO[Api](route)
      case route: RouteRepr.ReturningSSE[Api] => EndpointRepr.FromSSE[Api](route)

}
