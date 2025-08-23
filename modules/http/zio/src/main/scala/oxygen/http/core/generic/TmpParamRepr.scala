package oxygen.http.core.generic

import oxygen.http.core.*
import oxygen.http.core.RequestPathCodec
import oxygen.quoted.*
import scala.quoted.*

private[generic] sealed trait TmpParamRepr {

  val paramIdx: Int
  val name: String

  final def show(using Quotes): String = this match
    case TmpParamRepr.Path(valDef, name, paramIdx, codec)       => s"[$paramIdx] @path   $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"
    case TmpParamRepr.QueryParam(valDef, name, paramIdx, codec) => s"[$paramIdx] @query  $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"
    case TmpParamRepr.Header(valDef, name, paramIdx, codec)     => s"[$paramIdx] @header $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"
    case TmpParamRepr.Body(valDef, name, paramIdx, codec)       => s"[$paramIdx] @body   $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"
    case TmpParamRepr.CustomNonPath(valDef, paramIdx, codec)    => s"[$paramIdx] @body   $name: ${valDef.tpt.tpe.widen} =\n        ${codec.showAnsiCode}"

}
private[generic] object TmpParamRepr {

  final case class Path(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[RequestPathCodec[?]],
  ) extends TmpParamRepr

  sealed trait NonPath extends TmpParamRepr {

    final def ordIdx: Int = this match
      case _: QueryParam    => 2
      case _: Header        => 1
      case _: Body          => 3
      case _: CustomNonPath => 4

    def codec: Expr[RequestNonPathCodec[?]]

  }
  object NonPath {

    given Ordering[NonPath] =
      Ordering.by(p => (p.ordIdx, p.paramIdx))

  }

  final case class QueryParam(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec.ApplyPartialQueryParam[?]],
  ) extends NonPath

  final case class Header(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec.ApplyPartialHeader[?]],
  ) extends NonPath

  final case class Body(
      valDef: ValDef,
      name: String,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec.ApplyPartialBody[?]],
  ) extends NonPath

  final case class CustomNonPath(
      valDef: ValDef,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec[?]],
  ) extends NonPath {
    override val name: String = valDef.name
  }

}
