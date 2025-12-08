package oxygen.http.client.generic

import oxygen.http.client.*
import oxygen.http.core.*
import oxygen.http.core.generic.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

sealed trait EndpointRepr[Api] {

  val route: RouteRepr[Api]
  val classSym: Symbol
  import route.given

  private lazy val defDefSym: Symbol =
    classSym.declaredMethod(route.defDef.name) match
      case sym :: Nil => sym
      case Nil        => report.errorAndAbort(s"No symbol found for route?", route.defDef.pos)
      case syms       => report.errorAndAbort(s"Route returned multiple symbols: ${syms.mkString(", ")}", route.defDef.pos) // TODO (KR) : be able to figure out which is the correct one

  final type In = route.In
  type Out

  val outTpe: Type[Out]

  protected def makeImpl(using Quotes): Expr[DerivedClientEndpointImpl[In, Out]]

  final def withImpl[O: Type](f: EndpointRepr.WithImpl[Api] => Expr[O])(using Quotes): Expr[O] = {
    given Type[Out] = outTpe
    ValDef.companion.letExpr[DerivedClientEndpointImpl[In, Out], O](
      route.defDef.name + "__Impl",
      makeImpl,
      ValDef.ValType.LazyVal,
    ) { expr => f(EndpointRepr.WithImpl(this)(expr)) }
  }

  final def toDefinition(clientExpr: Expr[Client], implExpr: Expr[DerivedClientEndpointImpl[In, Out]])(using Quotes): DefDef = {
    given Type[Out] = outTpe
    DefDef.companion.apply(
      defDefSym,
      {
        case args :: Nil =>
          val terms: List[Term] = args.map(_.narrow[Term]("param args are not a term?"))
          val inExpr: Expr[In] = route.paramOrderTermsToInExpr(terms)
          val outExpr: Expr[Out] = '{ $implExpr.send($inExpr, $clientExpr) }
          outExpr.toTerm.some
        case argss =>
          report.errorAndAbort(s"Expected single param clause, found: ${argss.size}(${argss.map(_.size).mkString(", ")})")
      },
    )
  }

  protected final def extrasExpr(using Quotes): Expr[Client.RequestExtras] =
    '{
      Client.RequestExtras(
        apiName = ${ Expr(route.derivedApiName) },
        endpointName = ${ Expr(route.derivedEndpointName) },
      )
    }

  protected final def clientErrorHandlerExpr(using Quotes): Expr[ClientErrorHandler[route.ErrorOut]] =
    Implicits.searchOption[ClientErrorHandler[route.ErrorOut]].getOrElse { route.fail(s"No given instance found for: ${TypeRepr.of[ClientErrorHandler[route.ErrorOut]].showAnsiCode}") }

}
object EndpointRepr {

  final class WithImpl[Api](val endpointRepr: EndpointRepr[Api])(val impl: Expr[DerivedClientEndpointImpl[endpointRepr.In, endpointRepr.Out]]) {

    def toDefinition(clientExpr: Expr[Client])(using Quotes): DefDef =
      endpointRepr.toDefinition(clientExpr, impl)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromZIO
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class FromZIO[Api](val route: RouteRepr.ReturningZIO[Api], val classSym: Symbol) extends EndpointRepr[Api] {
    import route.given

    override type Out = ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]
    override val outTpe: Type[Out] = Type.of[Out]

    override protected def makeImpl(using Quotes): Expr[DerivedClientEndpointImpl[In, ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]]] =
      if route.envIsScoped then
        '{
          DerivedClientEndpointImpl.FromZIOScopedEnv[route.In, route.ErrorOut, route.SuccessOut](
            extras = $extrasExpr,
            requestCodec = ${ route.requestCodec },
            errorResponseCodec = ${ route.errorResponseCodec },
            successResponseCodec = ${ route.successResponseCodec },
            clientErrorHandler = $clientErrorHandlerExpr,
          )
        }.asExprOf[DerivedClientEndpointImpl[In, ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]]]
      else
        '{
          DerivedClientEndpointImpl.FromZIOAnyEnv[route.In, route.ErrorOut, route.SuccessOut](
            extras = $extrasExpr,
            requestCodec = ${ route.requestCodec },
            errorResponseCodec = ${ route.errorResponseCodec },
            successResponseCodec = ${ route.successResponseCodec },
            clientErrorHandler = $clientErrorHandlerExpr,
          )
        }.asExprOf[DerivedClientEndpointImpl[In, ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]]]

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromSSE
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class FromSSE[Api](val route: RouteRepr.ReturningSSE[Api], val classSym: Symbol) extends EndpointRepr[Api] {
    import route.given

    override type Out = ServerSentEvents[route.ErrorOut, route.SuccessOut]
    override val outTpe: Type[Out] = Type.of[Out]

    override protected def makeImpl(using Quotes): Expr[DerivedClientEndpointImpl[In, ServerSentEvents[route.ErrorOut, route.SuccessOut]]] =
      '{
        DerivedClientEndpointImpl.FromSSE[route.In, route.ErrorOut, route.SuccessOut](
          extras = $extrasExpr,
          requestCodec = ${ route.requestCodec },
          errorResponseCodec = ${ route.errorResponseCodec },
          schema = ${ route.successSchema },
          clientErrorHandler = $clientErrorHandlerExpr,
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Entry/Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply[Api](route: RouteRepr[Api], classSym: Symbol): EndpointRepr[Api] =
    route match
      case route: RouteRepr.ReturningZIO[Api] => EndpointRepr.FromZIO[Api](route, classSym)
      case route: RouteRepr.ReturningSSE[Api] => EndpointRepr.FromSSE[Api](route, classSym)

}
