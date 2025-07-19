package oxygen.http.client.generic

import oxygen.http.client.*
import oxygen.http.core.{*, given}
import oxygen.http.core.generic.*
import oxygen.http.model.*
import oxygen.meta.{*, given}
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

final class EndpointRepr[Api](val route: RouteRepr[Api], classSym: Symbol) {
  import route.given

  private val defDefSym: Symbol = classSym.declaredMethod(route.defDef.name).head // TODO (KR) : make this better

  private given Quotes = defDefSym.asQuotes

  // TODO (KR) : should cache path/param/body codecs in lazy vals as well
  def toDefinition(clientExpr: Expr[HttpClient]): DefDef =
    DefDef.companion.apply(
      defDefSym,
      {
        case args :: Nil =>
          val terms: Contiguous[Term] = args.into[Contiguous].map(_.narrow[Term]("param args are not a term?"))
          val termsAndParams: Contiguous[(Term, ParamRepr.FunctionArg)] = terms.zipExact(route.functionParams)
          functionImpl(clientExpr, termsAndParams).toTerm.some
        case argss => report.errorAndAbort(s"Expected single param clause, found: ${argss.size}(${argss.map(_.size).mkString(", ")})")
      },
    )

  private def functionImpl(clientExpr: Expr[HttpClient], args: Contiguous[(Term, ParamRepr.FunctionArg)]): Expr[ZIO[route.EnvOut, route.ErrorOut, route.SuccessOut]] =
    '{
      val request: HttpRequest = ${ toHttpRequest(args) }
      val sendResult: ZIO[Scope, HttpClientError, HttpResponse] = $clientExpr.send(request)
      val withConvertedClientError: ZIO[Scope, route.ErrorOut, HttpResponse] = sendResult.orDie // TODO (KR) : type-class for wrapping this
      val parsedResponse: ZIO[Scope, route.ErrorOut, route.SuccessOut] = withConvertedClientError.flatMap { response => ${ parseResponse('response) } }
      ${ route.convertR('parsedResponse) }
    }

  private def toHttpRequest(args: Contiguous[(Term, ParamRepr.FunctionArg)]): Expr[HttpRequest] = {
    val constPaths: Contiguous[ParamRepr.ConstPath] = route.pathParams.collect { case p: ParamRepr.ConstPath => p }
    val nonConstPaths: Contiguous[(Term, ParamRepr.NonConstPath)] = args.collect { case (t, p: ParamRepr.NonConstPath) => (t, p) }
    val queryParams: Contiguous[(Term, ParamRepr.QueryParam)] = args.collect { case (t, p: ParamRepr.QueryParam) => (t, p) }
    val headers: Contiguous[(Term, ParamRepr.Header)] = args.collect { case (t, p: ParamRepr.Header) => (t, p) }
    val body: Option[(Term, ParamRepr.Body)] = args.collectFirst { case (t, p: ParamRepr.Body) => (t, p) }

    val methodExpr: Expr[HttpMethod] = Expr(route.method)

    val tmpPaths: Contiguous[(ParamRepr.Path, Expr[Growable[String]])] =
      constPaths.map { p => (p, '{ Growable.single(${ Expr(p.path) }) }) } ++
        nonConstPaths.map { case (t, p) =>
          type P
          given Type[P] = p.typeRepr.asTypeOf
          val codecExpr: Expr[PathCodec[P]] = p.codec.asExprOf[PathCodec[P]] // TODO (KR) : cache
          val termExpr: Expr[P] = t.asExprOf[P]
          (p, '{ $codecExpr.encode($termExpr) })
        }

    // TODO (KR) : Maybe consider using mutable builders here?

    // TODO (KR) : Be more efficient, joining adjacent const params, and such.
    val pathExpr: Expr[Contiguous[String]] =
      '{
        ${ tmpPaths.sortBy(_._1.parseIdx).map(_._2).seqToExprOf[Growable] }.flatten.to[Contiguous]
      }

    def paramTuples(args: Contiguous[(Term, ParamRepr.ParamMap)]): Expr[Contiguous[(String, String)]] = {
      val exprs: Contiguous[Expr[Growable[(String, String)]]] =
        args.map { case (t, p) =>
          type P
          given Type[P] = p.typeRepr.asTypeOf
          val codecExpr: Expr[ParamCodec[P]] = p.codec.asExprOf[ParamCodec[P]] // TODO (KR) : cache
          val termExpr: Expr[P] = t.asExprOf[P]

          '{
            Growable.many($codecExpr.encode($termExpr)).map((${ Expr(p.name) }, _))
          }
        }

      exprs match {
        case Contiguous()     => '{ Contiguous.empty }
        case Contiguous(expr) => '{ $expr.to[Contiguous] }
        case exprs            => '{ ${ Growable.many(exprs).seqToExpr }.flatten.to[Contiguous] }
      }
    }

    val queryParamsExpr: Expr[Contiguous[(String, String)]] = paramTuples(queryParams)

    val headersExpr: Expr[Contiguous[(String, String)]] = paramTuples(headers)

    val bodyExpr: Expr[HttpBody] =
      body match {
        case Some((t, p)) =>
          type P
          given Type[P] = p.typeRepr.asTypeOf
          val codecExpr: Expr[BodyCodec[P]] = p.codec.asExprOf[BodyCodec[P]] // TODO (KR) : cache
          val termExpr: Expr[P] = t.asExprOf[P]
          '{ $codecExpr.encode($termExpr) }
        case None =>
          '{ HttpBody.emptyPlain }
      }

    val reqExpr: Expr[HttpRequest] =
      '{
        HttpRequest(
          method = $methodExpr,
          paths = $pathExpr,
          queryParams = QueryParams($queryParamsExpr),
          headers = Headers($headersExpr),
          body = $bodyExpr,
        )
      }

    reqExpr
  }

  private def parseResponse(responseExpr: Expr[HttpResponse]): Expr[ZIO[Scope, route.ErrorOut, route.SuccessOut]] = {
    val successCodesExpr: Expr[HttpCodes[route.SuccessOut]] = route.successCodes // TODO (KR) : cache
    val errorCodesExpr: Expr[HttpCodes[route.ErrorOut]] = route.errorCodes // TODO (KR) : cache
    val successBodyCodecExpr: Expr[ResponseCodec[route.SuccessOut]] = route.successResponseCodec // TODO (KR) : cache
    val errorBodyCodecExpr: Expr[ResponseCodec[route.ErrorOut]] = route.errorResponseCodec // TODO (KR) : cache

    // TODO (KR) : do better handling in error cases
    '{
      if ($successCodesExpr.possibleCodes.contains($responseExpr.statusCode))
        $successBodyCodecExpr
          .decode($responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode success response: $error") } // TODO (KR) : better error handling
      else if ($errorCodesExpr.possibleCodes.contains($responseExpr.statusCode))
        $errorBodyCodecExpr
          .decode($responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode error response: $error") } // TODO (KR) : better error handling
          .flip
      else if ($responseExpr.statusCode.is2xx) // TODO (KR) : special handling?
        $successBodyCodecExpr
          .decode($responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode success response: $error") } // TODO (KR) : better error handling
      else if ($responseExpr.statusCode.is4xxOr5xx) // TODO (KR) : special handling?
        $successBodyCodecExpr
          .decode($responseExpr.headers, $responseExpr.body)
          .catchAll { error => ZIO.dieMessage(s"Unable to decode success response: $error") } // TODO (KR) : better error handling
      else
        ZIO.dieMessage(s"Unexpected http response code (${$responseExpr.statusCode})") // TODO (KR) : better error handling
    }
  }

}
