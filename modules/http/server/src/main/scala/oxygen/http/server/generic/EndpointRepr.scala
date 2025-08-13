package oxygen.http.server.generic

import oxygen.http.core.{*, given}
import oxygen.http.core.generic.*
import oxygen.http.model.*
import oxygen.http.server.*
import oxygen.meta.{*, given}
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

final class EndpointRepr[Api](val route: RouteRepr[Api])(using Quotes) {
  import route.given

  private val serverErrorHandlerExpr: Expr[ServerErrorHandler[route.ErrorOut]] =
    Implicits.searchRequiredIgnoreMessage[ServerErrorHandler[route.ErrorOut]]

  private final case class Context(
      apiExpr: Expr[Api],
      routeHandler: Expr[RouteHandler[route.ErrorOut, route.SuccessOut]],
      requestContext: Expr[RequestContext],
  )
  private object Context {

    final case class Partial(
        apiExpr: Expr[Api],
        routeHandler: Expr[RouteHandler[route.ErrorOut, route.SuccessOut]],
    ) {

      def finish[A: Type](req: Expr[RequestContext])(f: Context => Expr[A]): Expr[A] =
        f(Context(apiExpr, routeHandler, req))

    }

    def expr[A: Type](apiExpr: Expr[Api])(f: Context.Partial => Expr[A]): Expr[A] =
      '{
        lazy val routeHandler: RouteHandler[route.ErrorOut, route.SuccessOut] =
          RouteHandler[route.ErrorOut, route.SuccessOut](
            errorResponseCodec = ${ route.errorResponseCodec },
            successResponseCodec = ${ route.successResponseCodec },
            errorCodes = ${ route.errorCodes },
            successCodes = ${ route.successCodes },
            serverErrorHandler = $serverErrorHandlerExpr,
          )

        ${ f(Context.Partial(apiExpr, 'routeHandler)) }
      }

  }

  private def makeReturn(
      ctx: Context,
      parsedTerms: List[Term],
  ): Expr[ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]] =
    ctx.apiExpr.toTerm.select(route.defDef.symbol).appliedToArgs(parsedTerms).asExprOf[ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]]

  private def mapReturn(
      ctx: Context,
      expr: Expr[ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]],
  ): Expr[URIO[route.EnvOut, HttpResponse]] =
    '{
      $expr.foldZIO(
        error => ${ ctx.routeHandler }.convertError(error),
        success => ZIO.succeed(${ ctx.routeHandler }.convertSuccess(success)),
      )
    }

  private def nonPathParseLoop(
      ctx: Context,
      nonPathParamsQueue: List[ParamRepr.NonPath],
      parsed: Growable[(ParamRepr.FunctionArg, Term)],
  ): Expr[URIO[route.EnvOut, HttpResponse]] =
    nonPathParamsQueue match {
      case head :: tail =>
        type P
        given Type[P] = head.typeRepr.asTypeOf

        def decodingFailure(source: DecodingFailure.Source, cause: Expr[DecodingFailure.Cause]): Expr[UIO[HttpResponse]] =
          '{ ${ ctx.routeHandler }.convertDecodingFailure(DecodingFailure(${ Expr(source) }, $cause)) }

        head match {
          case head: ParamRepr.QueryParam =>
            '{ // TODO (KR) : cache
              ${ head.codec.asExprOf[ParamCodec[P]] }.decode(${ ctx.requestContext }.request.queryParam(${ Expr(head.name) })) match {
                case Right(queryParamValue) => ${ nonPathParseLoop(ctx, tail, parsed :+ (head, ('queryParamValue).toTerm)) }
                case Left(cause)            => ${ decodingFailure(DecodingFailure.RequestSource.QueryParam(head.name), 'cause) }
              }
            }
          case head: ParamRepr.Header =>
            '{ // TODO (KR) : cache
              ${ head.codec.asExprOf[ParamCodec[P]] }.decode(${ ctx.requestContext }.request.header(${ Expr(head.name) })) match {
                case Right(headerValue) => ${ nonPathParseLoop(ctx, tail, parsed :+ (head, ('headerValue).toTerm)) }
                case Left(cause)        => ${ decodingFailure(DecodingFailure.RequestSource.Header(head.name), 'cause) }
              }
            }
          case head: ParamRepr.Body =>
            route.convertR {
              '{ // TODO (KR) : cache
                ${ head.codec.asExprOf[BodyCodec[P]] }
                  .decode(${ ctx.requestContext }.request.body)
                  .foldZIO(
                    cause => ${ decodingFailure(DecodingFailure.RequestSource.Body, 'cause) },
                    bodyValue => ${ nonPathParseLoop(ctx, tail, parsed :+ (head, ('bodyValue).toTerm)) },
                  )
              }
            }
        }
      case Nil =>
        val parsedTerms: List[Term] = parsed.to[List].sortBy(_._1.paramIdx).map(_._2)
        val unhandledExpr: Expr[ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]] = makeReturn(ctx, parsedTerms)
        val handledExpr: Expr[URIO[route.EnvOut, HttpResponse]] = mapReturn(ctx, unhandledExpr)
        handledExpr
    }

  private def pathParseLoop(
      ctx: Context,
      pathParamsQueue: List[ParamRepr.Path],
      remainingPathExpr: Expr[List[String]],
      parsed: Growable[(ParamRepr.NonConstPath, Term)],
  ): Expr[Option[URIO[route.EnvOut, HttpResponse]]] =
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
          ${ head.codec.asExprOf[PathCodec[P]] }.decode($remainingPathExpr).flatMap { case (pathValue, remainingPath) =>
            ${ pathParseLoop(ctx, tail, 'remainingPath, parsed :+ (head, ('pathValue).toTerm)) }
          }
        }
      case Nil =>
        '{
          if ($remainingPathExpr.isEmpty)
            Some(${ nonPathParseLoop(ctx, route.nonPathParams.toList, parsed) })
          else
            None
        }
    }

  private def toEndpointImpl(ctx: Context): Expr[Option[URIO[route.EnvOut, HttpResponse]]] =
    '{
      if (${ ctx.requestContext }.request.method == ${ Expr(route.method) })
        ${
          pathParseLoop(
            ctx,
            route.pathParams.toList,
            '{ ${ ctx.requestContext }.request.paths.toList },
            Growable.empty,
          )
        }
      else None
    }

  private val pathCodecParts: ArraySeq[Expr[PathCodec[?]]] =
    route.pathParams.map {
      case param: ParamRepr.ConstPath    => param.codec
      case param: ParamRepr.NonConstPath => param.codec
    }

  // TODO (KR) : should cache path/param/body codecs in lazy vals as well
  def toEndpoint(apiExpr: Expr[Api]): Expr[Endpoint] =
    Context.expr[Endpoint](apiExpr) { partial =>
      '{
        Endpoint(
          PathCodec.specsFor(${ Expr.ofSeq(pathCodecParts.toSeq) }*),
          req => ${ partial.finish('req)(toEndpointImpl) },
        )
      }
    }

}
