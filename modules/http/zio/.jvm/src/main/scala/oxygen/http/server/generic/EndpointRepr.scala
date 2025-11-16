package oxygen.http.server.generic

import oxygen.http.core.*
import oxygen.http.core.generic.{*, given}
import oxygen.http.schema.partial.RequestSchemaAggregator
import oxygen.http.server.*
import oxygen.meta.{*, given}
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*
import zio.http.{Body, Headers, QueryParams, Response}

final class EndpointRepr[Api](val route: RouteRepr[Api])(using Quotes) {
  import route.given

  private val serverErrorHandlerExpr: Expr[ServerErrorHandler[route.ErrorOut]] =
    Implicits.searchRequiredIgnoreExplanation[ServerErrorHandler[route.ErrorOut]]

  private final case class Context(
      apiExpr: Expr[Api],
      routeHandler: Expr[ResponseHandler[route.ErrorOut, route.SuccessOut]],
      requestContext: Expr[EndpointInput],
      queryParams: Expr[QueryParams],
      headers: Expr[Headers],
      body: Expr[Body],
  )
  private object Context {

    final case class Partial(
        apiExpr: Expr[Api],
        routeHandler: Expr[ResponseHandler[route.ErrorOut, route.SuccessOut]],
    ) {

      def finish[A: Type](req: Expr[EndpointInput])(f: Context => Expr[A]): Expr[A] =
        '{
          val queryParams: QueryParams = $req.request.queryParameters
          val headers: Headers = $req.request.headers
          val body: Body = $req.request.body
          ${ f(Context(apiExpr, routeHandler, req, 'queryParams, 'headers, 'body)) }
        }

    }

    def expr[A: Type](apiExpr: Expr[Api])(f: Context.Partial => Expr[A]): Expr[A] =
      '{
        lazy val routeHandler: ResponseHandler[route.ErrorOut, route.SuccessOut] =
          ResponseHandler[route.ErrorOut, route.SuccessOut](
            errorResponseCodec = ${ route.errorResponseCodec },
            successResponseCodec = ${ route.successResponseCodec },
            serverErrorHandler = $serverErrorHandlerExpr,
          )

        ${ f(Context.Partial(apiExpr, 'routeHandler)) }
      }

  }

  private def makeReturn(
      ctx: Context,
      parsedTerms: List[Term],
  ): Expr[ZIO[Scope, route.ErrorOut, route.SuccessOut]] =
    ctx.apiExpr.toTerm.select(route.defDef.symbol).appliedToArgs(parsedTerms).asExprOf[ZIO[Scope, route.ErrorOut, route.SuccessOut]]

  private def mapReturn(
      ctx: Context,
      expr: Expr[ZIO[Scope, route.ErrorOut, route.SuccessOut]],
  ): Expr[URIO[Scope, Response]] =
    '{
      $expr.foldCauseZIO(
        error => ${ ctx.routeHandler }.convertCause(error, ${ ctx.requestContext }.exposeInternalErrors),
        success => ZIO.succeed(${ ctx.routeHandler }.successResponse(success)),
      )
    }

  private def nonPathParseLoop(
      ctx: Context,
      nonPathParamsQueue: List[ParamRepr.NonPath],
      parsed: Growable[(ParamRepr.FunctionArg, Term)],
  ): Expr[URIO[Scope, Response]] =
    nonPathParamsQueue match {
      case head :: tail =>
        type P
        given Type[P] = head.typeRepr.asTypeOf

        '{ // TODO (KR) : cache
          ${ head.codec.asExprOf[RequestNonPathCodec[P]] }
            .decode(${ ctx.queryParams }, ${ ctx.headers }, ${ ctx.body })
            .foldZIO(
              ${ ctx.routeHandler }.convertDecodingFailure(_),
              value => ${ nonPathParseLoop(ctx, tail, parsed :+ (head, ('value).toTerm)) },
            )
        }
      case Nil =>
        val parsedTerms: List[Term] = parsed.to[List].sortBy(_._1.paramIdx).map(_._2)
        val unhandledExpr: Expr[ZIO[Scope, route.ErrorOut, route.SuccessOut]] = '{
          ${ makeReturn(ctx, parsedTerms) } @@ ZIOAspect.annotated("endpoint", ${ Expr(s"${route.derivedApiName}.${route.derivedEndpointName}") })
        }
        val handledExpr: Expr[URIO[Scope, Response]] = mapReturn(ctx, unhandledExpr)
        handledExpr
    }

  private def pathParseLoop(
      ctx: Context,
      pathParamsQueue: List[ParamRepr.Path],
      remainingPathExpr: Expr[List[String]],
      parsed: Growable[(ParamRepr.NonConstPath, Term)],
  ): Expr[Option[URIO[route.EnvOut, Response]]] =
    pathParamsQueue match {
      case (head: ParamRepr.ConstPath) :: tail =>
        '{ // TODO (KR) : cache
          ${ head.codec }.decode($remainingPathExpr).flatMap { case (_, remainingPath) =>
            ${ pathParseLoop(ctx, tail, 'remainingPath, parsed) }
          }
        }
      case (head: ParamRepr.NonConstPath) :: tail =>
        type P
        given Type[P] = head.typeRepr.asTypeOf

        '{ // TODO (KR) : cache
          ${ head.codec.asExprOf[RequestPathCodec[P]] }.decode($remainingPathExpr).flatMap { case (pathValue, remainingPath) =>
            ${ pathParseLoop(ctx, tail, 'remainingPath, parsed :+ (head, ('pathValue).toTerm)) }
          }
        }
      case Nil =>
        '{
          if ($remainingPathExpr.isEmpty)
            Some(${ route.convertEnv(nonPathParseLoop(ctx, route.nonPathParams.toList, parsed)) })
          else
            None
        }
    }

  private def toEndpointImpl(ctx: Context): Expr[Option[URIO[route.EnvOut, Response]]] =
    '{
      if (${ ctx.requestContext }.request.method == ${ Expr(route.method) })
        ${
          pathParseLoop(
            ctx,
            route.pathParams.toList,
            '{ ${ ctx.requestContext }.request.url.path.segments.toList },
            Growable.empty,
          )
        }
      else None
    }

  // TODO (KR) : should cache path/param/body codecs in lazy vals as well
  def toEndpoint(apiExpr: Expr[Api]): Expr[Endpoint] =
    Context.expr[Endpoint](apiExpr) { partial =>
      val apiNameExpr: Expr[String] = Expr(route.derivedApiName)
      val endpointNameExpr: Expr[String] = Expr(route.derivedEndpointName)

      '{
        Endpoint(
          apiName = $apiNameExpr.some,
          endpointName = $endpointNameExpr,
          requestSchema = RequestSchemaAggregator.unsafeBuild(
            $apiNameExpr,
            $endpointNameExpr,
            ${ Expr(route.method) },
          )(
            ${ Expr.ofSeq(route.allParamsInParseOrder.map(_.codec)) }*,
          ),
          successResponseSchema = ${ partial.routeHandler }.successResponseCodec.unsafeBuild($apiNameExpr, $endpointNameExpr),
          errorResponseSchema = ${ partial.routeHandler }.errorResponseCodec.unsafeBuild($apiNameExpr, $endpointNameExpr),
          doc = ${ Expr(route.defDefDoc) },
          handle = req => ${ partial.finish('req)(toEndpointImpl) },
        )
      }
    }

}
