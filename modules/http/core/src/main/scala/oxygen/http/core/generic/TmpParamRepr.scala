package oxygen.http.core.generic

import oxygen.http.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] sealed trait TmpParamRepr {

  val paramIdx: Int

  final def show(using Quotes): String = this match
    case TmpParamRepr.Path(valDef, name, paramIdx, codec)       => s"[$paramIdx] @path   $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"
    case TmpParamRepr.QueryParam(valDef, name, paramIdx, codec) => s"[$paramIdx] @query  $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"
    case TmpParamRepr.Header(valDef, name, paramIdx, codec)     => s"[$paramIdx] @header $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"
    case TmpParamRepr.Body(valDef, name, paramIdx, codec)       => s"[$paramIdx] @body   $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"

}
private[generic] object TmpParamRepr {

  final case class Path(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[PathCodec[?]],
  ) extends TmpParamRepr

  sealed trait NonPath extends TmpParamRepr {

    final def ordIdx: Int = this match
      case _: QueryParam => 2
      case _: Header     => 1
      case _: Body       => 3

  }
  object NonPath {

    given Ordering[NonPath] =
      Ordering.by(p => (p.ordIdx, p.paramIdx))

  }

  final case class QueryParam(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[ParamCodec[?]],
  ) extends NonPath

  final case class Header(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[ParamCodec[?]],
  ) extends NonPath

  final case class Body(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[BodyCodec[?]],
  ) extends NonPath

}
