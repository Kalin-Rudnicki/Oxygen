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

  final def toIndentedString: IndentedString =
    this match {
      case ParamRepr.ConstPath(parseIdx, path) =>
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

  sealed trait Path extends ParamRepr

  final case class ConstPath(
      parseIdx: Int,
      path: String,
  ) extends Path {

    def codec(using Quotes): Expr[PathCodec[Unit]] = '{ PathCodec.Const(${ Expr(path) }) }

  }

  final case class NonConstPath(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[PathCodec[?]],
  ) extends ParamRepr.Path,
        ParamRepr.FunctionArg

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      NonPath
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NonPath extends ParamRepr.FunctionArg

  sealed trait ParamMap extends NonPath {
    val codec: Expr[ParamCodec[?]]
  }

  final case class QueryParam(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[ParamCodec[?]],
  ) extends ParamMap

  final case class Header(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[ParamCodec[?]],
  ) extends ParamMap

  final case class Body(
      valDef: ValDef,
      name: String,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[BodyCodec[?]],
  ) extends NonPath

}
