package oxygen.http.core.generic

import oxygen.http.core.*
import oxygen.http.core.partial.*
import oxygen.http.model.internal.*
import oxygen.http.schema.partial.*
import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.schema.*
import oxygen.zio.instances.given
import scala.annotation.tailrec
import scala.quoted.*
import zio.*
import zio.http.{Method, QueryParams, Status}

sealed abstract class RouteRepr[Api](
    final val defDef: DefDef,
    _apiType: Type[Api],
    _errorOutTypeRepr: TypeRepr,
    _successOutTypeRepr: TypeRepr,
    _quotes: Quotes,
) {

  protected final val apiTypeRepr: TypeRepr = _apiType.toTypeRepr

  type In
  type ErrorOut
  type SuccessOut

  final given apiType: Type[Api] = _apiType
  final given quotes: Quotes = _quotes

  final given errorOutType: Type[ErrorOut] = _errorOutTypeRepr.asTypeOf
  final given successOutType: Type[SuccessOut] = _successOutTypeRepr.asTypeOf

  // TODO (KR) : Try to emit errors at DefDef/ValDef pos. Seems to emit in a garbage location when `retainTrees` is off.
  //           : Maybe its possible to detect this with some sort of `_.idx == 0`, and add `use retainTrees flag` helper message.
  final def fail(msg: String): Nothing =
    report.errorAndAbort(
      s"""
         |Error deriving generic HTTP RouteRepr.
         |     API: ${apiTypeRepr.showAnsiCode}
         |Function: ${defDef.name}
         |
         |""".stripMargin + msg,
      defDef.pos,
    )
  final def failParam(valDef: ValDef, msg: String): Nothing =
    report.errorAndAbort(
      s"""
         |Error deriving generic HTTP RouteRepr.
         |     API: ${apiTypeRepr.showAnsiCode}
         |Function: ${defDef.name}
         |   Param: ${valDef.name}
         |
         |""".stripMargin + msg,
      valDef.pos,
    )

  final val derivedApiName: String = apiTypeRepr.typeSymbol.annotations.optionalOfValue[apiName].fold(apiTypeRepr.typeSymbol.name)(_.name)
  final val derivedEndpointName: String = defDef.symbol.annotations.optionalOfValue[endpointName].fold(defDef.name)(_.name)
  final val defDefDoc: Option[String] = defDef.symbol.annotations.optionalOfValue[httpDoc].map(_.doc)

  private val routeAnnotValue: route =
    defDef.symbol.annotations.optionalOf[route] match
      case Some(Expr(value)) => value
      case Some(expr)        => fail(s"invalid @route.___ annotation\n\n${expr.showAnsiCode}")
      case None              => fail("missing @route.___ annotation")

  final val httpMethod: Option[Method] = routeAnnotValue.method.someWhen(_ != Method.ANY)

  private val termParamClause: TermParamClause = defDef.paramss match
    case (tpc: TermParamClause) :: Nil => tpc
    case _                             => fail("Function must have single parameter group with no type params")

  private val valDefs: List[ValDef] = termParamClause.params
  private val (rawPath, rawQueryParams): (String, String) = routeAnnotValue.url.split('?').toList match
    case path :: Nil       => (path, "")
    case path :: qp :: Nil => (path, qp.trim)
    case _                 => report.errorAndAbort("multiple '?' in path..?")
  private val pathList: List[String] = rawPath.split('/').toList.map(_.trim).filter(_.nonEmpty)
  val queryParams: Option[QueryParams] = Option.when(rawQueryParams.nonEmpty)(QueryParams.decode(rawQueryParams))

  if pathList.contains("..") then report.errorAndAbort("path contains \"..\"")

  private val unsortedParamReprs: List[ParamRepr[?]] = {
    @tailrec
    def loop(
        parseIdx: Int,
        paramIdx: Int,
        pathQueue: List[String],
        paramQueue: List[ParamRepr[?]],
        rConstStack: List[String],
        rOutStack: List[ParamRepr[?]],
    ): List[ParamRepr[?]] =
      (pathQueue, paramQueue, rConstStack) match {
        case (("%" :: _) | Nil, _, _ :: _) =>
          val const = ParamRepr.ConstPath(parseIdx, NonEmptyList.unsafeFromList(rConstStack.reverse), Type.of[Unit])
          loop(parseIdx + 1, paramIdx, pathQueue, paramQueue, Nil, const :: rOutStack)
        case ("%" :: pathTail, (paramHead: ParamRepr.PathLike[?]) :: paramTail, Nil) => loop(parseIdx + 1, paramIdx + 1, pathTail, paramTail, Nil, paramHead.withParseIndex(parseIdx) :: rOutStack)
        case ("%" :: _, (paramHead: ParamRepr.NonPathLike[?]) :: paramTail, Nil)     => loop(parseIdx + 1, paramIdx + 1, pathQueue, paramTail, Nil, paramHead.withParseIndex(parseIdx) :: rOutStack)
        case ("%" :: _, Nil, Nil)                                                    => fail("path has more % than @param.path")
        case (pathHead :: pathTail, _, _)                                            => loop(parseIdx, paramIdx, pathTail, paramQueue, pathHead :: rConstStack, rOutStack)
        case (Nil, (_: ParamRepr.PathLike[?]) :: _, Nil)                             => fail("path has more @param.path than %")
        case (Nil, (paramHead: ParamRepr.NonPathLike[?]) :: paramTail, Nil)          => loop(parseIdx + 1, paramIdx + 1, pathQueue, paramTail, Nil, paramHead.withParseIndex(parseIdx) :: rOutStack)
        case (Nil, Nil, Nil)                                                         => rOutStack.reverse
      }

    loop(0, 0, pathList, valDefs.zipWithIndex.map(parseParamRepr), Nil, Nil)
  }

  final val allParamsInParseOrder: ArraySeq[ParamRepr[?]] =
    ArraySeq
      .from(unsortedParamReprs)
      .sorted(using
        Ordering
          .by[ParamRepr[?], Int] {
            case _: ParamRepr.AppliedMethod        => 1
            case _: ParamRepr.PathLike[?]          => 2
            case _: ParamRepr.AppliedHeader[?]     => 3
            case _: ParamRepr.AppliedQueryParam[?] => 4
            case _: ParamRepr.CustomNonPath[?]     => 5
            case _: ParamRepr.AppliedBody[?]       => 6
          }
          .orElseBy(_.parseIdx),
      )
      .zipWithIndex
      .map { case (repr, idx) => repr.withParseIndex(idx) }

  final val functionParamsInParseOrder: ArraySeq[ParamRepr.FunctionArg[?]] =
    allParamsInParseOrder.collect { case fa: ParamRepr.FunctionArg[?] => fa }

  final val functionParamsInParamOrder: ArraySeq[ParamRepr.FunctionArg[?]] =
    functionParamsInParseOrder.sortBy(_.paramIdx)

  (httpMethod.nonEmpty, allParamsInParseOrder.exists { case _: ParamRepr.AppliedMethod => true; case _ => false }) match
    case (true, false)  => ()
    case (false, true)  => ()
    case (false, false) => fail("function must have a fixed method, or include a `@param.method _: Method` param")
    case (true, true)   => fail("function can not have a fixed method and a `@param.method _: Method` param")

  /**
    * Imagine a function that looks like
    * (@param.path p1: P1T, @param.body body: BodyT, @param.query query: QueryT, @param.header header: HeaderT, @param.path: p2: P2T)
    *
    * the `param order` tuple for this type is: (P1T, BodyT, QueryT, HeaderT, P2T)
    * while the `parse order` tuple is:         (P1T, P2T, HeaderT, QueryT, BodyT)
    *
    * This reasoning here is that:
    * 1. I definitely want to make sure I have parsed the path before trying to parse non-path params (especially the body)
    * 2. I still would rather parse non-body params before parsing a body
    * 3. Arbitrarily, if I had a missing query param or missing header, I would rather know about the missing header first.
    *
    * The [[In]] type represents the `parse order` type mentioned above (we needed to pick one).
    * The below functions assist in re-ordering params. For example, if you had an `Expr[In]`, getting the param order tuple out of that would return:
    * `List(in._1, in._5, in._4, in._3, in._2)`
    * And passing a sequence of terms to create an `Expr[In]` looks like:
    * `(p1, p2, header, query, body)`
    *
    * Because we are working with [[Term]]/[[Expr]] here, the generated code is extremely efficient, there is no repeated tupling/un-tupling.
    */
  private val inUnderlying: UnderlyingConverter[In] =
    UnderlyingConverter.of[In](allParamsInParseOrder.collect { case p: ParamRepr.FunctionArg[?] => p.typeRepr }.toList)

  final def inExprToParseOrderTerms(in: Expr[In])(using Quotes): List[Term] =
    inUnderlying.splitExpr(in)

  final def parseOrderTermsToInExpr(terms: List[Term])(using Quotes): Expr[In] =
    inUnderlying.joinExpr(terms)

  private val paramOrderReverseLookup: ArraySeq[Int] = functionParamsInParamOrder.zipWithIndex.sortBy(_._1.parseIdx).map(_._2)
  final def inExprToParamOrderTerms(in: Expr[In])(using Quotes): List[Term] = inUnderlying.splitExpr(in).zip(paramOrderReverseLookup).sortBy(_._2).map(_._1)

  private val parseOrderReverseLookup: ArraySeq[Int] = functionParamsInParseOrder.zipWithIndex.sortBy(_._1.paramIdx).map(_._2)
  final def paramOrderTermsToInExpr(terms: List[Term])(using Quotes): Expr[In] = inUnderlying.joinExpr(terms.zip(parseOrderReverseLookup).sortBy(_._2).map(_._1))

  final given inType: Type[In] = inUnderlying.aType

  final val methodType: MethodType = MethodType.companion.apply(valDefs.map(_.name))(_ => valDefs.map(_.tpt.tpe), _ => defDef.returnTpt.tpe)

  final def requestCodec(using Quotes): Expr[RequestCodec[In]] =
    httpMethod match
      case Some(httpMethod) =>
        '{
          RequestCodec.CustomWithConstMethod[In](
            ${ Expr(httpMethod) },
            ${ baseRequestCodec },
          )
        }
      case None => baseRequestCodec

  private def baseRequestCodec(using Quotes): Expr[RequestCodec[In]] =
    requestCodecRec(allParamsInParseOrder.toList, Nil, queryParams)

  /////// Abstract ///////////////////////////////////////////////////////////////

  def toIndentedString: IndentedString

  /////// Util ///////////////////////////////////////////////////////////////

  private def requestCodecRec(
      queue: List[ParamRepr[?]],
      rParamStack: List[ParamRepr.WithCodec[?]],
      queryParams: Option[QueryParams],
  )(using Quotes): Expr[RequestCodec[In]] =
    queue match {
      case head :: tail =>
        head.usingCodec[RequestCodec[In]] { wc => requestCodecRec(tail, wc :: rParamStack, queryParams) }
      case Nil =>
        val params: List[ParamRepr.WithCodec[?]] = rParamStack.reverse
        '{
          new RequestCodec.Custom[In] {

            private val codecParts: List[RequestCodec[?]] =
              ${ params.map(_.codecExpr).seqToExpr }

            override lazy val sources: List[RequestDecodingFailure.Source] = codecParts.flatMap(_.sources)
            override lazy val schemaAggregator: RequestSchemaAggregator = codecParts.foldLeft(RequestSchemaAggregator.empty) { (acc, c) => acc >>> c.schemaAggregator }

            @tailrec
            private def matchesPathLoop(
                path: List[String],
                queue: List[RequestCodec[?]],
            ): Option[List[String]] =
              queue match {
                case head :: tail =>
                  head.matchesPathPatternInternal(path) match {
                    case Some(newPath) => matchesPathLoop(newPath, tail)
                    case None          => None
                  }
                case Nil =>
                  path.some
              }

            override def matchesPathPatternInternal(path: List[String]): Option[List[String]] =
              matchesPathLoop(path, codecParts)

            override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[In] =
              ${ decodeRec('remainingPaths, 'request, params, Nil) }

            override def encodeInternal(value: In, acc: RequestBuilder): RequestBuilder = {
              val tmpBuilder: RequestBuilder = ${ encodeRec(inExprToParseOrderTerms('value), 'acc, params) }
              ${
                queryParams match {
                  case Some(queryParams) => '{ tmpBuilder.addQueryParams(${ Growable.many(queryParams.map.toSeq).map(Expr(_)).seqToExpr }) }
                  case None              => 'tmpBuilder
                }
              }
            }

          }
        }
    }

  private def decodeRec(
      remainingPathExpr: Expr[List[String]],
      requestExpr: Expr[ReceivedRequest],
      queue: List[ParamRepr.WithCodec[?]],
      rParamStack: List[Term],
  )(using Quotes): Expr[IntermediateRequestParseResult[In]] =
    queue match {
      case _head :: tail =>
        type T
        val head: ParamRepr.WithCodec[T] = _head.asInstanceOf[ParamRepr.WithCodec[T]]
        given Type[T] = head.paramRepr.tpe

        head.paramRepr match {
          case _: ParamRepr.ConstPath =>
            '{
              ${ head.codecExpr }.decodeInternal($remainingPathExpr, $requestExpr).flatMap { (_, remainingPath) =>
                ${ decodeRec('remainingPath, requestExpr, tail, rParamStack) }
              }
            }
          case _: ParamRepr.FunctionArg[T] =>
            '{
              ${ head.codecExpr }.decodeInternal($remainingPathExpr, $requestExpr).flatMap { (value, remainingPath) =>
                ${ decodeRec('remainingPath, requestExpr, tail, 'value.toTerm :: rParamStack) }
              }
            }
        }
      case Nil =>
        '{
          IntermediateRequestParseResult.Success(
            value = ${ parseOrderTermsToInExpr(rParamStack.reverse) },
            remainingPath = $remainingPathExpr,
          )
        }
    }

  @tailrec
  private def encodeRec(
      termQueue: List[Term],
      requestBuilderExpr: Expr[RequestBuilder],
      queue: List[ParamRepr.WithCodec[?]],
  )(using Quotes): Expr[RequestBuilder] =
    queue match {
      case _head :: tail =>
        _head.paramRepr match {
          case _: ParamRepr.ConstPath =>
            val head: ParamRepr.WithCodec[Unit] = _head.asInstanceOf[ParamRepr.WithCodec[Unit]]
            encodeRec(
              termQueue,
              '{ ${ head.codecExpr }.encodeInternal((), $requestBuilderExpr) },
              tail,
            )
          case _: ParamRepr.FunctionArg[?] =>
            type T
            val head: ParamRepr.WithCodec[T] = _head.asInstanceOf[ParamRepr.WithCodec[T]]
            given Type[T] = head.paramRepr.tpe

            termQueue match {
              case tHead :: tTail =>
                encodeRec(
                  tTail,
                  '{ ${ head.codecExpr }.encodeInternal(${ tHead.asExprOf[T] }, $requestBuilderExpr) },
                  tail,
                )
              case Nil =>
                report.errorAndAbort("Internal defect: Exhausted terms but not ParamRepr?")
            }
        }
      case Nil if termQueue.nonEmpty =>
        report.errorAndAbort("Internal defect: Exhausted ParamRepr but not terms?")
      case Nil =>
        requestBuilderExpr
    }

  protected final def deriveResponseCodec[T: Type](typeType: String, defaultCode: Status)(using Quotes): Expr[ResponseCodec[T]] =
    Implicits.searchOption[ResponseCodec[T]].getOrElse {
      val responseCodecNoStatus: Expr[ResponseCodecNoStatus[T]] =
        Implicits.searchOption[ResponseCodecNoStatus[T]].getOrElse {
          fail(
            s"""Unable to derive ${TypeRepr.of[ResponseCodec[T]].showAnsiCode} for $typeType response.
               |
               |Adding a given instance for one of the following typeclasses should fix the problem:
               |1. oxygen.schema.PlainTextSchema
               |2. oxygen.schema.JsonSchema
               |3. oxygen.http.core.partial.ResponseCodecNoStatus
               |4. oxygen.http.core.ResponseCodec
               |""".stripMargin,
          )
        }

      val optionalStatusCodes: Option[Expr[StatusCodes[T]]] = Implicits.searchOption[StatusCodes[T]]
      val statusCodes: Expr[StatusCodes[T]] = optionalStatusCodes.getOrElse { '{ StatusCodes.Exact(${ Expr(defaultCode) }) } }

      '{ ResponseCodec.Standard($statusCodes, $responseCodecNoStatus) }
    }

  private def parseParamRepr(valDef: ValDef, paramIdx: Int): ParamRepr[?] = {
    val annots = valDef.symbol.annotations

    val paramAnnotExpr: Expr[param] =
      annots.optionalOf[param].getOrElse { failParam(valDef, "missing @param.___ annotation") }

    type A
    given aType: Type[A] = valDef.tpt.tpe.widen.asTypeOf

    def valDefDoc: Option[String] = annots.optionalOfValue[httpDoc].map(_.doc)
    def valDefName: String = valDef.name
    def valDefHeaderName: String = valDef.name.camelToSnake.split('_').map(_.capitalize).mkString("-")

    def summonInstance[T[_]: Type]: Expr[T[A]] =
      Implicits.searchOption[T[A]].getOrElse(failParam(valDef, s"No given instance found for ${TypeRepr.of[T[A]].showAnsiCode}"))

    paramAnnotExpr match {

      // =====| custom |=====
      case '{ new `param`.`path`.`custom`() }    => ParamRepr.CustomPath[A](valDef, paramIdx, paramIdx, summonInstance[RequestCodec.PathLike], aType)
      case '{ new `param`.`nonPath`.`custom`() } => ParamRepr.CustomNonPath[A](valDef, paramIdx, paramIdx, summonInstance[RequestCodec.NonPathLike], aType)

      // =====| applied method |=====
      case '{ new `param`.`method`() }                => ParamRepr.AppliedMethod(valDef, valDefName, valDefDoc, paramIdx, paramIdx, Type.of[zio.http.Method])
      case '{ new `param`.`method`(${ Expr(name) }) } => ParamRepr.AppliedMethod(valDef, name, valDefDoc, paramIdx, paramIdx, Type.of[zio.http.Method])

      // =====| applied path |=====
      case '{ new `param`.`path`() }                        => ParamRepr.AppliedPath[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialPathCodec], aType)
      case '{ new `param`.`path`(${ Expr(name) }) }         => ParamRepr.AppliedPath[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialPathCodec], aType)
      case '{ new `param`.`path`.`plain`() }                => ParamRepr.AppliedPath[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialPathCodec.Plain], aType)
      case '{ new `param`.`path`.`plain`(${ Expr(name) }) } => ParamRepr.AppliedPath[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialPathCodec.Plain], aType)
      case '{ new `param`.`path`.`json`() }                 => ParamRepr.AppliedPath[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialPathCodec.Json], aType)
      case '{ new `param`.`path`.`json`(${ Expr(name) }) }  => ParamRepr.AppliedPath[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialPathCodec.Json], aType)

      // =====| applied query |=====
      case '{ new `param`.`query`() }                        => ParamRepr.AppliedQueryParam[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec], aType)
      case '{ new `param`.`query`(${ Expr(name) }) }         => ParamRepr.AppliedQueryParam[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec], aType)
      case '{ new `param`.`query`.`plain`() }                => ParamRepr.AppliedQueryParam[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Plain], aType)
      case '{ new `param`.`query`.`plain`(${ Expr(name) }) } => ParamRepr.AppliedQueryParam[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Plain], aType)
      case '{ new `param`.`query`.`json`() }                 => ParamRepr.AppliedQueryParam[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Json], aType)
      case '{ new `param`.`query`.`json`(${ Expr(name) }) }  => ParamRepr.AppliedQueryParam[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Json], aType)

      // =====| applied header |=====
      case '{ new `param`.`header`() }                        => ParamRepr.AppliedHeader[A](valDef, valDefHeaderName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec], aType)
      case '{ new `param`.`header`(${ Expr(name) }) }         => ParamRepr.AppliedHeader[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec], aType)
      case '{ new `param`.`header`.`plain`() }                => ParamRepr.AppliedHeader[A](valDef, valDefHeaderName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Plain], aType)
      case '{ new `param`.`header`.`plain`(${ Expr(name) }) } => ParamRepr.AppliedHeader[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Plain], aType)
      case '{ new `param`.`header`.`json`() }                 => ParamRepr.AppliedHeader[A](valDef, valDefHeaderName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Json], aType)
      case '{ new `param`.`header`.`json`(${ Expr(name) }) }  => ParamRepr.AppliedHeader[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialParamCodec.Json], aType)

      // =====| applied body |=====
      case '{ new `param`.`body`() }                        => ParamRepr.AppliedBody[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialBodyCodec], aType)
      case '{ new `param`.`body`(${ Expr(name) }) }         => ParamRepr.AppliedBody[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialBodyCodec], aType)
      case '{ new `param`.`body`.`plain`() }                => ParamRepr.AppliedBody[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialBodyCodec.Plain], aType)
      case '{ new `param`.`body`.`plain`(${ Expr(name) }) } => ParamRepr.AppliedBody[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialBodyCodec.Plain], aType)
      case '{ new `param`.`body`.`json`() }                 => ParamRepr.AppliedBody[A](valDef, valDefName, valDefDoc, paramIdx, paramIdx, summonInstance[PartialBodyCodec.Json], aType)
      case '{ new `param`.`body`.`json`(${ Expr(name) }) }  => ParamRepr.AppliedBody[A](valDef, name, valDefDoc, paramIdx, paramIdx, summonInstance[PartialBodyCodec.Json], aType)

      // =====| unknown |=====
      case _ => report.errorAndAbort(s"Unable to extract param annotation expr: ${paramAnnotExpr.showAnsiCode}")

    }
  }

}
object RouteRepr {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ReturningZIO
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class ReturningZIO[Api](
      defDef: DefDef,
      _apiType: Type[Api],
      _errorOutTypeRepr: TypeRepr,
      _successOutTypeRepr: TypeRepr,
      _quotes: Quotes,
  )(
      val envIsScoped: Boolean,
      _envTypeRepr: TypeRepr,
  ) extends RouteRepr[Api](defDef, _apiType, _errorOutTypeRepr, _successOutTypeRepr, _quotes) {

    type EnvOut >: Scope

    given envOutType: Type[EnvOut] = _envTypeRepr.asTypeOf

    def convertZioRType[E: Type, A: Type](effect: Expr[ZIO[Scope, E, A]])(using Quotes): Expr[ZIO[EnvOut, E, A]] =
      if envIsScoped then effect.asExprOf[ZIO[EnvOut, E, A]]
      else '{ ZIO.scoped { $effect } }

    def errorResponseCodec(using Quotes): Expr[ResponseCodec[ErrorOut]] =
      deriveResponseCodec[ErrorOut]("error", Status.BadRequest)
    def successResponseCodec(using Quotes): Expr[ResponseCodec[SuccessOut]] =
      deriveResponseCodec[SuccessOut]("error", Status.Ok)

    override def toIndentedString: IndentedString =
      IndentedString.section(
        s"[ZIO Endpoint] < env = ${envOutType.toTypeRepr.showAnsiCode}, error = ${errorOutType.toTypeRepr.showAnsiCode}, success = ${successOutType.toTypeRepr.showAnsiCode}> ${defDef.name}:",
      )(
        IndentedString.section("params:")(allParamsInParseOrder.map(_.toIndentedString)*),
        IndentedString.section("function params:")(functionParamsInParamOrder.map(_.toIndentedString)*),
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ReturningSSE
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class ReturningSSE[Api](
      defDef: DefDef,
      _apiType: Type[Api],
      _errorOutTypeRepr: TypeRepr,
      _successOutTypeRepr: TypeRepr,
      _quotes: Quotes,
  ) extends RouteRepr[Api](defDef, _apiType, _errorOutTypeRepr, _successOutTypeRepr, _quotes) {

    def errorResponseCodec(using Quotes): Expr[ResponseCodec[ErrorOut]] =
      deriveResponseCodec[ErrorOut]("error", Status.BadRequest)
    def successSchema(using Quotes): Expr[AnySchemaT[SuccessOut]] = // TODO (KR) : have the ability to prefer json schema over plain-text schema
      Implicits.searchOption[PlainTextSchema[SuccessOut]].getOrElse {
        Implicits.searchOption[JsonSchema[SuccessOut]].getOrElse {
          this.fail(
            s"""Unable to derive ResponseCodec for SEE success type: ${TypeRepr.of[SuccessOut].showAnsiCode}.
              |
               |Oxygen looks for typeclass instances in the following order:
               |1. ${TypeRepr.of[oxygen.schema.PlainTextSchema[SuccessOut]].showAnsiCode}
               |2. ${TypeRepr.of[oxygen.schema.JsonSchema[SuccessOut]].showAnsiCode}
              |""".stripMargin,
          )
        }
      }

    override def toIndentedString: IndentedString =
      IndentedString.section(
        s"[SSE Endpoint] < error = ${errorOutType.toTypeRepr.showAnsiCode}, success = ${successOutType.toTypeRepr.showAnsiCode}> ${defDef.name}:",
      )(
        IndentedString.section("params:")(allParamsInParseOrder.map(_.toIndentedString)*),
        IndentedString.section("function params:")(functionParamsInParamOrder.map(_.toIndentedString)*),
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ReturningLines
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class ReturningLines[Api](
      defDef: DefDef,
      _apiType: Type[Api],
      _errorOutTypeRepr: TypeRepr,
      _successOutTypeRepr: TypeRepr,
      _quotes: Quotes,
  ) extends RouteRepr[Api](defDef, _apiType, _errorOutTypeRepr, _successOutTypeRepr, _quotes) {

    def errorResponseCodec(using Quotes): Expr[ResponseCodec[ErrorOut]] =
      deriveResponseCodec[ErrorOut]("error", Status.BadRequest)
    def successSchema(using Quotes): Expr[AnySchemaT[SuccessOut]] = // TODO (KR) : have the ability to prefer json schema over plain-text schema
      Implicits.searchOption[PlainTextSchema[SuccessOut]].getOrElse {
        Implicits.searchOption[JsonSchema[SuccessOut]].getOrElse {
          this.fail(
            s"""Unable to derive ResponseCodec for LineStream success type: ${TypeRepr.of[SuccessOut].showAnsiCode}.
              |
               |Oxygen looks for typeclass instances in the following order:
               |1. ${TypeRepr.of[oxygen.schema.PlainTextSchema[SuccessOut]].showAnsiCode}
               |2. ${TypeRepr.of[oxygen.schema.JsonSchema[SuccessOut]].showAnsiCode}
              |""".stripMargin,
          )
        }
      }

    override def toIndentedString: IndentedString =
      IndentedString.section(
        s"[LineStream Endpoint] < error = ${errorOutType.toTypeRepr.showAnsiCode}, success = ${successOutType.toTypeRepr.showAnsiCode}> ${defDef.name}:",
      )(
        IndentedString.section("params:")(allParamsInParseOrder.map(_.toIndentedString)*),
        IndentedString.section("function params:")(functionParamsInParamOrder.map(_.toIndentedString)*),
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      derive
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def fail[Api: Type](defDef: DefDef)(msg: String)(using Quotes): Nothing =
    report.errorAndAbort(
      s"""
         |Error deriving generic HTTP RouteRepr.
         |     API: ${TypeRepr.of[Api].showAnsiCode}
         |Function: ${defDef.name}
         |
         |""".stripMargin + msg,
      defDef.pos,
    )

  private def deriveShouldIgnore(defDef: DefDef): Boolean =
    defDef.name.contains("$default$")

  private val supportedTypeString: Seq[String] =
    Seq(
      "ZIO[Scope | Any, _, _]",
      "ServerSentEvents[_, _]",
    )

  private def deriveRequired[Api: Type as apiTpe](defDef: DefDef)(using quotes: Quotes): RouteRepr[Api] =
    defDef.returnTpt.tpe.widen.asType match
      case '[ZIO[Any, e, a]]         => ReturningZIO(defDef, apiTpe, TypeRepr.of[e], TypeRepr.of[a], quotes)(false, TypeRepr.of[Any])
      case '[ZIO[Scope, e, a]]       => ReturningZIO(defDef, apiTpe, TypeRepr.of[e], TypeRepr.of[a], quotes)(true, TypeRepr.of[Scope])
      case '[ServerSentEvents[e, a]] => ReturningSSE(defDef, apiTpe, TypeRepr.of[e], TypeRepr.of[a], quotes)
      case '[LineStream[e, a]]       => ReturningLines(defDef, apiTpe, TypeRepr.of[e], TypeRepr.of[a], quotes)
      case '[ZIO[r, _, _]]           => fail[Api](defDef)(s"Function returns a ZIO, but R type is not `Scope` or `Any`: ${TypeRepr.of[r].showAnsiCode}")
      case _ => fail[Api](defDef)(s"Function returns unknown type.\nExpected one of:${supportedTypeString.map { s => s"\n  - $s" }.mkString}\nGot: ${apiTpe.toTypeRepr.showAnsiCode}")

  def derive[Api: Type as apiTpe](defDef: DefDef)(using Quotes): Option[RouteRepr[Api]] =
    Option.when(!deriveShouldIgnore(defDef)) { deriveRequired[Api](defDef) }

}
