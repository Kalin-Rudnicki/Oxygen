package oxygen.http.core.generic

import oxygen.http.core.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

sealed trait ParamRepr extends Product {

  /**
    * Order in which this [[ParamRepr]] will be parsed from the HTTP request.
    */
  val parseIdx: Int

  val codec: Expr[RequestPathCodec[?] | RequestNonPathCodec[?]]

  final def toIndentedString: IndentedString =
    this match {
      case ParamRepr.ConstPath(parseIdx, path, _) =>
        s"Param.ConstPath(parseIdx = $parseIdx, ${path.unesc})"
      case self: ParamRepr.FunctionArg =>
        s"Param.${self.productPrefix}(parseIdx = ${self.parseIdx}, paramIdx = ${self.paramIdx}, ${self.name}[${self.typeRepr.showAnsiCode}])"
    }

}
object ParamRepr {

  sealed trait FunctionArg extends ParamRepr {

    /**
      * Order in which this [[FunctionArg]] will be passed to the function impl once all [[ParamRepr]]s are parsed.
      */
    val paramIdx: Int
    val valDef: ValDef
    val name: String

    final lazy val typeRepr: TypeRepr = valDef.tpt.tpe.widen

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Path
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Path extends ParamRepr {
    override val codec: Expr[RequestPathCodec[?]]
  }

  final case class ConstPath(
      parseIdx: Int,
      path: String,
      codec: Expr[RequestPathCodec.ConstSingle],
  ) extends Path

  final case class NonConstPath(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[RequestPathCodec[?]],
  ) extends ParamRepr.Path,
        ParamRepr.FunctionArg

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      NonPath
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NonPath extends ParamRepr.FunctionArg {
    override val codec: Expr[RequestNonPathCodec[?]]
  }

  sealed trait ParamMap extends NonPath

  final case class QueryParam(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec.ApplyPartialQueryParam[?]],
  ) extends ParamMap

  final case class Header(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec.ApplyPartialHeader[?]],
  ) extends ParamMap

  final case class Body(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec.ApplyPartialBody[?]],
  ) extends NonPath

  final case class Custom(
      valDef: ValDef,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[RequestNonPathCodec[?]],
  ) extends NonPath {
    val name: String = valDef.name
  }

}
