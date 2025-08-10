package oxygen.http.core.generic

import oxygen.http.core.*
import oxygen.http.model.*
import oxygen.meta.given
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

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

  def convertR[E: Type, A: Type](effect: Expr[ZIO[Scope, E, A]])(using Quotes): Expr[ZIO[EnvOut, E, A]] =
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
  val method: HttpMethod.Standard = routeAnnotValue.method

  if (pathList.contains(".."))
    report.errorAndAbort("path contains \"..\"")

  private val paths: Contiguous[(Int, Option[String])] =
    pathList.into[Contiguous].zipWithIndex.map {
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

    paramAnnotValue match {
      case _: param.path =>
        val decoder: Expr[PathCodec[A]] = summonInstance[PathCodec]
        TmpParamRepr.Path(valDef, valDef.name, i, decoder)
      case _: param.query.plain =>
        val decoder: Expr[ParamCodec[A]] = summonInstance[ParamCodec.Plain]
        TmpParamRepr.QueryParam(valDef, valDef.name, i, decoder)
      case _: param.query.json =>
        val decoder: Expr[ParamCodec[A]] = summonInstance[ParamCodec.Json]
        TmpParamRepr.QueryParam(valDef, valDef.name, i, decoder)
      case _: param.header.plain =>
        val decoder: Expr[ParamCodec[A]] = summonInstance[ParamCodec.Plain]
        TmpParamRepr.Header(valDef, valDef.name.camelToSnake.split('_').map(_.capitalize).mkString("-"), i, decoder)
      case _: param.header.json =>
        val decoder: Expr[ParamCodec[A]] = summonInstance[ParamCodec.Json]
        TmpParamRepr.Header(valDef, valDef.name.capitalize, i, decoder)
      case _: param.body.plain =>
        val decoder: Expr[BodyCodec[A]] = summonInstance[BodyCodec.Plain]
        TmpParamRepr.Body(valDef, valDef.name, i, decoder)
      case _: param.body.json =>
        val decoder: Expr[BodyCodec[A]] = summonInstance[BodyCodec.Json]
        TmpParamRepr.Body(valDef, valDef.name, i, decoder)
    }
  }

  private val tmpParamReprs: Contiguous[TmpParamRepr] =
    valDefs.into[Contiguous].zipWithIndex.map(parseTmpParamRepr)

  private val tmpPathReprs: Contiguous[TmpParamRepr.Path] =
    tmpParamReprs.collect { case p: TmpParamRepr.Path => p }

  private val tmpNonPathReprs: Contiguous[TmpParamRepr.NonPath] =
    tmpParamReprs.collect { case p: TmpParamRepr.NonPath => p }

  tmpNonPathReprs.collect { case TmpParamRepr.Body(_, name, _, _) => name } match {
    case Contiguous()  => ()
    case Contiguous(_) => ()
    case bodies        => fail(s"more than 1 body (${bodies.mkString(", ")})")
  }

  private val idxMap: Map[Int, Int] =
    paths.collect { case (i, None) => i }.zipWithIndex.toMap

  if (idxMap.size != tmpPathReprs.length)
    fail(s"number of % and @param.path do not match (${idxMap.size} != ${tmpPathReprs.length})")

  val pathParams: Contiguous[ParamRepr.Path] =
    paths.map {
      case (i, Some(const)) =>
        ParamRepr.ConstPath(i, const)
      case (i, None) =>
        val tmpPath: TmpParamRepr.Path = tmpPathReprs.at(idxMap(i))
        ParamRepr.NonConstPath(tmpPath.valDef, tmpPath.name, i, tmpPath.paramIdx, tmpPath.codec)
    }

  val nonPathParams: Contiguous[ParamRepr.NonPath] =
    tmpNonPathReprs.sorted.zipWithIndexFrom(pathParams.length).map { case (np, i) =>
      np match {
        case TmpParamRepr.QueryParam(valDef, name, paramIdx, decoder) =>
          ParamRepr.QueryParam(valDef, name, i, paramIdx, decoder)
        case TmpParamRepr.Header(valDef, name, paramIdx, decoder) =>
          ParamRepr.Header(valDef, name, i, paramIdx, decoder)
        case TmpParamRepr.Body(valDef, name, paramIdx, decoder) =>
          ParamRepr.Body(valDef, name, i, paramIdx, decoder)
      }
    }

  val functionParams: Contiguous[ParamRepr.FunctionArg] =
    (pathParams ++ nonPathParams).collect { case fa: ParamRepr.FunctionArg => fa }.sortBy(_.paramIdx)

  val errorResponseCodec: Expr[ResponseCodec[ErrorOut]] =
    Implicits.searchOption[ResponseCodec[ErrorOut]].getOrElse(fail(s"No given ResponseCodec for error type ${TypeRepr.of[ErrorOut].showAnsiCode}"))
  val successResponseCodec: Expr[ResponseCodec[SuccessOut]] =
    Implicits.searchOption[ResponseCodec[SuccessOut]].getOrElse(fail(s"No given ResponseCodec for success type ${TypeRepr.of[SuccessOut].showAnsiCode}"))

  val errorCodes: Expr[HttpCodes[ErrorOut]] =
    Implicits.searchOption[HttpCodes[ErrorOut]].getOrElse('{ HttpCodes.Const(HttpCode.BadRequest) }) // TODO (KR) : should this have a default like this..?
  val successCodes: Expr[HttpCodes[SuccessOut]] =
    Implicits.searchOption[HttpCodes[SuccessOut]].getOrElse('{ HttpCodes.Const(HttpCode.Ok) })

  def toIndentedString: IndentedString =
    IndentedString.section(s"${defDef.name}:")(
      IndentedString.section("paths params:")(pathParams.map(_.toIndentedString).toSeq*),
      IndentedString.section("non-paths params:")(nonPathParams.map(_.toIndentedString).toSeq*),
      IndentedString.section("function params:")(functionParams.map(_.toIndentedString).toSeq*),
    )

}
object RouteRepr {

  def derive[Api: Type as apiTpe](defDef: DefDef)(using Quotes): RouteRepr[Api] =
    new RouteRepr[Api](defDef, apiTpe)

}
