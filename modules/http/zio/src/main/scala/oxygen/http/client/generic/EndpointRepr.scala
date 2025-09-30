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

  private val defDefSym: Symbol = classSym.declaredMethod(route.defDef.name).head // TODO (KR) : make this better

  private given Quotes = defDefSym.asQuotes

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
    val successBodyCodecExpr: Expr[ResponseCodec[route.SuccessOut]] = route.successResponseCodec // TODO (KR) : cache
    val errorBodyCodecExpr: Expr[ResponseCodec[route.ErrorOut]] = route.errorResponseCodec // TODO (KR) : cache

    // TODO (KR) : do better handling in error cases
    '{
      if ($successBodyCodecExpr.canLikelyDecode($responseExpr.status))
        $successBodyCodecExpr
          .decode($responseExpr.status, $responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode success response: $error") } // TODO (KR) : better error handling
      else if ($errorBodyCodecExpr.canLikelyDecode($responseExpr.status))
        $errorBodyCodecExpr
          .decode($responseExpr.status, $responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode error response: $error") } // TODO (KR) : better error handling
          .flip
      else if ($responseExpr.status.isSuccess) // TODO (KR) : special handling?
        $successBodyCodecExpr
          .decode($responseExpr.status, $responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode success response: $error") } // TODO (KR) : better error handling
      else if ($responseExpr.status.isError) // TODO (KR) : special handling?
        $errorBodyCodecExpr
          .decode($responseExpr.status, $responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode error response: $error") } // TODO (KR) : better error handling
          .flip
      else
        ZIO.dieMessage(s"Unexpected http response code (${$responseExpr.status})") // TODO (KR) : better error handling
    }
  }

}
