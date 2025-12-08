package oxygen.http.client.generic

import oxygen.http.client.*
import oxygen.http.core.*
import oxygen.http.core.generic.{*, given}
import oxygen.meta.given
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*
import zio.http.Response

final class EndpointRepr[Api](val route: RouteRepr[Api], classSym: Symbol) {
  import route.given

  private val defDefSym: Symbol = classSym.declaredMethod(route.defDef.name) match
    case sym :: Nil => sym
    case Nil        => report.errorAndAbort(s"No symbol found for route?", route.defDef.pos)
    case syms       => report.errorAndAbort(s"Route returned multiple symbols: ${syms.mkString(", ")}", route.defDef.pos) // TODO (KR) : be able to figure out which is the correct one

  private given Quotes = defDefSym.asQuotes

  // TODO (KR) : cache in generated class
  private val clientErrorHandlerExpr: Expr[ClientErrorHandler[route.ErrorOut]] =
    Implicits.searchRequiredIgnoreExplanation[ClientErrorHandler[route.ErrorOut]]

  // TODO (KR) : should cache path/param/body codecs in lazy vals as well
  def toDefinition(clientExpr: Expr[Client]): DefDef =
    DefDef.companion.apply(
      defDefSym,
      {
        case args :: Nil =>
          val terms: ArraySeq[Term] = args.toArraySeq.map(_.narrow[Term]("param args are not a term?"))
          val termsAndParams: ArraySeq[(Term, ParamRepr.FunctionArg)] = terms.zipExact(route.functionParamsInParamOrder)
          functionImpl(clientExpr, termsAndParams).toTerm.some
        case argss => report.errorAndAbort(s"Expected single param clause, found: ${argss.size}(${argss.map(_.size).mkString(", ")})")
      },
    )

  private def functionImpl(clientExpr: Expr[Client], args: ArraySeq[(Term, ParamRepr.FunctionArg)]): Expr[ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]] =
    '{
      // TODO (KR) : re-add metrics
      //             val metricLabels: Set[MetricLabel] = Set(MetricLabel("oxygen.api-name", ${ Expr(route.apiName) }), MetricLabel("oxygen.endpoint-name", ${ Expr(route.routeName) }))
      ZIO.suspendSucceed {
        val request: SendRequest = ${ toHttpRequest(args) }
        val sendResult: ZIO[Scope, Throwable, Response] = $clientExpr.send(request, $extrasExpr)
        val withConvertedClientError: ZIO[Scope, route.ErrorOut, Response] = sendResult.orDie // TODO (KR) : type-class for wrapping this
        val parsedResponse: ZIO[Scope, route.ErrorOut, route.SuccessOut] = withConvertedClientError.flatMap { response => ${ parseResponse('response) } }
        ${ route.convertEnv('parsedResponse) }
        // TODO (KR) : re-add metrics
      } //             @@ HttpClientMetrics.endpointDuration.tagged(metricLabels).toAspect
    }

  private def extrasExpr: Expr[Client.RequestExtras] =
    '{
      Client.RequestExtras(
        apiName = ${ Expr(route.derivedApiName) },
        endpointName = ${ Expr(route.derivedEndpointName) },
      )
    }

  private def toHttpRequest(args: ArraySeq[(Term, ParamRepr.FunctionArg)]): Expr[SendRequest] = {
    val constPaths: ArraySeq[ParamRepr.ConstPath] = route.pathParams.collect { case p: ParamRepr.ConstPath => p }
    val nonConstPaths: ArraySeq[(Term, ParamRepr.NonConstPath)] = args.collect { case (t, p: ParamRepr.NonConstPath) => (t, p) }
    val nonPaths: ArraySeq[(Term, ParamRepr.NonPath)] = args.collect { case (t, p: ParamRepr.NonPath) => (t, p) }

    val appliedConstPath: ArraySeq[(Expr[SendRequest.AppliedValue[?]], Int)] =
      constPaths.map { p => ('{ SendRequest.AppliedValue.Path(${ p.codec }, ()) }, p.parseIdx) }
    val appliedNonConstPaths: ArraySeq[(Expr[SendRequest.AppliedValue[?]], Int)] =
      nonConstPaths.map { case (t, p) =>
        type P
        given Type[P] = p.typeRepr.asTypeOf

        val tt: Expr[P] = t.asExprOf[P]
        val tc: Expr[RequestPathCodec[P]] = p.codec.asExprOf[RequestPathCodec[P]]

        ('{ SendRequest.AppliedValue.Path($tc, $tt) }, p.parseIdx)
      }
    val appliedNonPaths: ArraySeq[(Expr[SendRequest.AppliedValue[?]], Int)] =
      nonPaths.map { case (t, p) =>
        type P
        given Type[P] = p.typeRepr.asTypeOf

        val tt: Expr[P] = t.asExprOf[P]
        val tc: Expr[RequestNonPathCodec[P]] = p.codec.asExprOf[RequestNonPathCodec[P]]

        ('{ SendRequest.AppliedValue.NonPath($tc, $tt) }, p.parseIdx)
      }

    val appliedValues: ArraySeq[Expr[SendRequest.AppliedValue[?]]] =
      (appliedConstPath ++ appliedNonConstPaths ++ appliedNonPaths).sortBy(_._2).map(_._1)

    val reqExpr: Expr[SendRequest] =
      '{
        val (path, queryParams, headers, body) =
          SendRequest.make(${ Expr.ofSeq(appliedValues) }*)

        SendRequest(
          method = ${ Expr(route.method) },
          path = path,
          queryParams = queryParams,
          headers = headers,
          body = body,
        )
      }

    reqExpr
  }

  private def parseResponse(responseExpr: Expr[Response]): Expr[ZIO[Scope, route.ErrorOut, route.SuccessOut]] = {
    val successBodyCodecExpr: Expr[ResponseCodec[route.SuccessOut]] = route.successResponseCodec // TODO (KR) : cache in generated class
    val errorBodyCodecExpr: Expr[ResponseCodec[route.ErrorOut]] = route.errorResponseCodec // TODO (KR) : cache in generated class

    '{
      lazy val successBodyCodec: ResponseCodec[route.SuccessOut] = $successBodyCodecExpr
      lazy val errorBodyCodec: ResponseCodec[route.ErrorOut] = $errorBodyCodecExpr
      lazy val clientErrorHandler: ClientErrorHandler[route.ErrorOut] = $clientErrorHandlerExpr

      val unhandled: ZIO[Scope, route.ErrorOut, route.SuccessOut] =
        if successBodyCodec.canLikelyDecode($responseExpr.status) then //
          successBodyCodec.decodeSuccess($responseExpr.status, $responseExpr.headers, $responseExpr.body)(clientErrorHandler)
        else if errorBodyCodec.canLikelyDecode($responseExpr.status) then //
          errorBodyCodec.decodeError($responseExpr.status, $responseExpr.headers, $responseExpr.body)(clientErrorHandler)
        else if $responseExpr.status.isSuccess then // TODO (KR) : special handling?
          ZIO.logInfo(s"Received unexpected (success) status code (${$responseExpr.status}), attempting to decode as success...") *>
            successBodyCodec.decodeSuccess($responseExpr.status, $responseExpr.headers, $responseExpr.body)(clientErrorHandler)
        else if $responseExpr.status.isError then // TODO (KR) : special handling?
          ZIO.logInfo(s"Received unexpected (error) status code (${$responseExpr.status}), attempting to decode as error...") *>
            errorBodyCodec.decodeError($responseExpr.status, $responseExpr.headers, $responseExpr.body)(clientErrorHandler)
        else //
          ZIO.dieMessage(s"Unexpected http response code (${$responseExpr.status})")

      unhandled.mapErrorCause { cause =>
        cause.failureOrCause match {
          case Left(_)      => cause
          case Right(cause) => cause.foldContext(())(clientErrorHandler)
        }
      }
    }
  }

}
