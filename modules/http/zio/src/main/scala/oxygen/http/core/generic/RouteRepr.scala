package oxygen.http.core.generic

import oxygen.http.core.*
import oxygen.http.core.partial.*
import oxygen.meta.given
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*
import zio.http.{Method, Status}

final class RouteRepr[Api] private (val defDef: DefDef, _apiType: Type[Api])(using Quotes) {

  given apiType: Type[Api] = _apiType
  private val apiTypeRepr: TypeRepr = TypeRepr.of[Api]

  type EnvOut >: Scope
  type ErrorOut
  type SuccessOut

  // TODO (KR) : Try to emit errors at DefDef/ValDef pos. Seems to emit in a garbage location when `retainTrees` is off.
  //           : Maybe its possible to detect this with some sort of `_.idx == 0`, and add `use retainTrees flag` helper message.
  def fail(msg: String): Nothing =
    report.errorAndAbort(s"${apiTypeRepr.showAnsiCode}.${defDef.name} : $msg")
  def failParam(valDef: ValDef, msg: String): Nothing =
    report.errorAndAbort(s"${apiTypeRepr.showAnsiCode}.${defDef.name}.${valDef.name} : $msg")

  val derivedApiName: String = apiTypeRepr.typeSymbol.annotations.optionalOfValue[apiName].fold(apiTypeRepr.typeSymbol.name)(_.name)
  val derivedEndpointName: String = defDef.symbol.annotations.optionalOfValue[endpointName].fold(defDef.name)(_.name)
  val defDefDoc: Option[String] = defDef.symbol.annotations.optionalOfValue[httpDoc].map(_.doc)

  private val termParamClause: TermParamClause = defDef.paramss match
    case (tpc: TermParamClause) :: Nil => tpc
    case _                             => fail("expected single parameter clause with no type params")

  private val zioTycon: TypeRepr = TypeRepr.of[ZIO[?, ?, ?]].narrow[AppliedType].tycon
  private val defDefRet: AppliedType = defDef.returnTpt.tpe.dealias.narrow[AppliedType]

  private val (rTpe, eTpe, aTpe) = defDefRet.args match
    case List(rTpe, eTpe, aTpe) if defDefRet.tycon =:= zioTycon => (rTpe, eTpe, aTpe)
    case _                                                      => fail(s"return type is not a ZIO: ${defDefRet.showAnsiCode}")

  private val isScoped: Boolean =
    if (rTpe =:= TypeRepr.of[Any]) false
    else if (rTpe =:= TypeRepr.of[Scope]) true
    else fail("R type is not `Any`/`Scope`")

  def convertEnv[E: Type, A: Type](effect: Expr[ZIO[Scope, E, A]])(using Quotes): Expr[ZIO[EnvOut, E, A]] =
    if (isScoped) effect.asExprOf[ZIO[EnvOut, E, A]]
    else '{ ZIO.scoped { $effect } }

  given envOutType: Type[EnvOut] = rTpe.asTypeOf
  given errorOutType: Type[ErrorOut] = eTpe.asTypeOf
  given successOutType: Type[SuccessOut] = aTpe.asTypeOf

  private val valDefs: List[ValDef] = termParamClause.params

  def methodType: MethodType =
    MethodType.companion.apply(valDefs.map(_.name))(_ => valDefs.map(_.tpt.tpe), _ => defDef.returnTpt.tpe)

  private val routeAnnotValue: route =
    defDef.symbol.annotations.optionalOf[route] match
      case Some(Expr(value)) => value
      case Some(expr)        => fail(s"invalid @route.___ annotation\n\n${expr.showAnsiCode}")
      case None              => fail("missing @route.___ annotation")
  private val pathList: List[String] = routeAnnotValue.url.split('/').toList.map(_.trim).filter(_.nonEmpty)
  val method: Method = routeAnnotValue.method

  if (pathList.contains(".."))
    report.errorAndAbort("path contains \"..\"")

  private val paths: ArraySeq[(Int, Option[String])] =
    pathList.toArraySeq.zipWithIndex.map {
      case ("%", i) => (i, None)
      case (s, i)   => (i, s.some)
    }

  private def parseTmpParamRepr(valDef: ValDef, i: Int): TmpParamRepr = {
    val paramAnnotValue: param =
      valDef.symbol.annotations.optionalOf[param] match
        case Some(Expr(value)) => value
        case Some(expr)        => fail(s"invalid @param.___ annotation\n\n${expr.showAnsiCode}")
        case None              => fail("missing @param.___ annotation")

    type A
    given Type[A] = valDef.tpt.tpe.widen.asTypeOf

    // TODO (KR) : allow for custom name

    def summonInstance[T[_]: Type]: Expr[T[A]] =
      Implicits.searchOption[T[A]].getOrElse(failParam(valDef, s"No given instance found for ${TypeRepr.of[T[A]].showAnsiCode}"))

    def valDefDocExpr: Expr[Option[String]] = Expr(valDef.symbol.annotations.optionalOfValue[httpDoc].map(_.doc))

    paramAnnotValue match {
      case paramAnnotValue: param.PathLike =>
        // TODO (KR) : support custom name
        val name = valDef.name
        val inst: Expr[RequestPathCodec[A]] =
          paramAnnotValue match {
            case param.path() =>
              '{ RequestPathCodec.ApplyPartial(${ summonInstance[PartialPathCodec] }, ${ Expr(name) }, $valDefDocExpr) }
            case param.path.plain() =>
              '{ RequestPathCodec.ApplyPartial(${ summonInstance[PartialPathCodec.Plain] }, ${ Expr(name) }, $valDefDocExpr) }
            case param.path.json() =>
              '{ RequestPathCodec.ApplyPartial(${ summonInstance[PartialPathCodec.Json] }, ${ Expr(name) }, $valDefDocExpr) }
            case param.path.custom() =>
              summonInstance[RequestPathCodec]
          }
        TmpParamRepr.Path(valDef, name, i, inst)
      case paramAnnotValue: param.NonPathLike =>
        paramAnnotValue match {
          case paramAnnotValue: param.QueryLike =>
            // TODO (KR) : support custom name
            val name = valDef.name
            val inst: Expr[PartialParamCodec[A]] =
              paramAnnotValue match {
                case param.query()       => summonInstance[PartialParamCodec]
                case param.query.plain() => summonInstance[PartialParamCodec.Plain]
                case param.query.json()  => summonInstance[PartialParamCodec.Json]
              }
            TmpParamRepr.QueryParam(valDef, name, i, '{ RequestNonPathCodec.ApplyPartialQueryParam($inst, ${ Expr(name) }, $valDefDocExpr) })
          case paramAnnotValue: param.HeaderLike =>
            // TODO (KR) : support custom name
            val name = valDef.name.camelToSnake.split('_').map(_.capitalize).mkString("-")
            val inst: Expr[PartialParamCodec[A]] =
              paramAnnotValue match {
                case param.header()       => summonInstance[PartialParamCodec]
                case param.header.plain() => summonInstance[PartialParamCodec.Plain]
                case param.header.json()  => summonInstance[PartialParamCodec.Json]
              }
            TmpParamRepr.Header(valDef, name, i, '{ RequestNonPathCodec.ApplyPartialHeader($inst, ${ Expr(name) }, $valDefDocExpr) })
          case paramAnnotValue: param.BodyLike =>
            // TODO (KR) : support custom name
            val name = valDef.name
            val inst: Expr[PartialBodyCodec[A]] =
              paramAnnotValue match {
                case param.body()       => summonInstance[PartialBodyCodec]
                case param.body.plain() => summonInstance[PartialBodyCodec.Plain]
                case param.body.json()  => summonInstance[PartialBodyCodec.Json]
              }
            TmpParamRepr.Body(valDef, name, i, '{ RequestNonPathCodec.ApplyPartialBody($inst, ${ Expr(name) }, $valDefDocExpr) })
          case param.nonPath.custom() =>
            val inst: Expr[RequestNonPathCodec[A]] = summonInstance[RequestNonPathCodec]
            TmpParamRepr.CustomNonPath(valDef, i, inst)
        }
    }
  }

  private val tmpParamReprs: ArraySeq[TmpParamRepr] =
    valDefs.toArraySeq.zipWithIndex.map(parseTmpParamRepr)

  private val tmpPathReprs: ArraySeq[TmpParamRepr.Path] =
    tmpParamReprs.collect { case p: TmpParamRepr.Path => p }

  private val tmpNonPathReprs: ArraySeq[TmpParamRepr.NonPath] =
    tmpParamReprs.collect { case p: TmpParamRepr.NonPath => p }

  tmpNonPathReprs.collect { case TmpParamRepr.Body(_, name, _, _) => name } match {
    case ArraySeq()  => ()
    case ArraySeq(_) => ()
    case bodies      => fail(s"more than 1 body (${bodies.mkString(", ")})")
  }

  private val idxMap: Map[Int, Int] =
    paths.collect { case (i, None) => i }.zipWithIndex.toMap

  if (idxMap.size != tmpPathReprs.length)
    fail(s"number of % and @param.path do not match (${idxMap.size} != ${tmpPathReprs.length})")

  val pathParams: ArraySeq[ParamRepr.Path] =
    paths.map {
      case (i, Some(const)) =>
        ParamRepr.ConstPath(i, const, '{ RequestPathCodec.ConstSingle(${ Expr(const) }) })
      case (i, None) =>
        val tmpPath: TmpParamRepr.Path = tmpPathReprs(idxMap(i))
        ParamRepr.NonConstPath(tmpPath.valDef, tmpPath.name, i, tmpPath.paramIdx, tmpPath.codec)
    }

  val nonPathParams: ArraySeq[ParamRepr.NonPath] =
    tmpNonPathReprs.sorted.zipWithIndexFrom(pathParams.length).map { case (np, i) =>
      np match {
        case TmpParamRepr.QueryParam(valDef, name, paramIdx, decoder) =>
          ParamRepr.QueryParam(valDef, name, i, paramIdx, decoder)
        case TmpParamRepr.Header(valDef, name, paramIdx, decoder) =>
          ParamRepr.Header(valDef, name, i, paramIdx, decoder)
        case TmpParamRepr.Body(valDef, name, paramIdx, decoder) =>
          ParamRepr.Body(valDef, name, i, paramIdx, decoder)
        case TmpParamRepr.CustomNonPath(valDef, paramIdx, decoder) =>
          ParamRepr.Custom(valDef, i, paramIdx, decoder)
      }
    }

  val allParamsInParseOrder: ArraySeq[ParamRepr] =
    (pathParams ++ nonPathParams).sortBy(_.parseIdx)

  val functionParamsInParamOrder: ArraySeq[ParamRepr.FunctionArg] =
    allParamsInParseOrder.collect { case fa: ParamRepr.FunctionArg => fa }.sortBy(_.paramIdx)

  private def deriveResponseCodec[T: Type](typeType: String, defaultCode: Status): Expr[ResponseCodec[T]] =
    Implicits.searchOption[ResponseCodec[T]].getOrElse {
      val responseCodecNoStatus: Expr[ResponseCodecNoStatus[T]] =
        Implicits.searchOption[ResponseCodecNoStatus[T]].getOrElse {
          report.errorAndAbort(
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

  val errorResponseCodec: Expr[ResponseCodec[ErrorOut]] =
    deriveResponseCodec[ErrorOut]("error", Status.BadRequest)
  val successResponseCodec: Expr[ResponseCodec[SuccessOut]] =
    deriveResponseCodec[SuccessOut]("error", Status.Ok)

  def toIndentedString: IndentedString =
    IndentedString.section(s"${defDef.name}:")(
      IndentedString.section("paths params:")(pathParams.map(_.toIndentedString).toSeq*),
      IndentedString.section("non-paths params:")(nonPathParams.map(_.toIndentedString).toSeq*),
      IndentedString.section("function params:")(functionParamsInParamOrder.map(_.toIndentedString).toSeq*),
    )

}
object RouteRepr {

  def derive[Api: Type as apiTpe](defDef: DefDef)(using Quotes): RouteRepr[Api] =
    new RouteRepr[Api](defDef, apiTpe)

}
