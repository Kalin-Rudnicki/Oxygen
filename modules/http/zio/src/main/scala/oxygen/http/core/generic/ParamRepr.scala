package oxygen.http.core.generic

import oxygen.http.core.*
import oxygen.http.core.partial.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

sealed trait ParamRepr[T] extends Product {

  /**
    * Order in which this [[ParamRepr]] will be parsed from the HTTP request.
    */
  val parseIdx: Int
  val tpe: Type[T]
  def makeCodec(using Quotes): Expr[RequestCodec.PathLike[T] | RequestCodec.NonPathLike[T]]

  final def usingCodec[O: Type](f: ParamRepr.WithCodec[T] => Expr[O])(using Quotes): Expr[O] = {
    ValDef.companion.letExpr[RequestCodec[T], O](
      s"requestCodec_param_$parseIdx",
      makeCodec,
      ValDef.ValType.LazyVal,
    ) { expr => f(ParamRepr.WithCodec(this)(expr)) }
  }

  final given Type[T] = tpe

  final def toIndentedString: IndentedString =
    this match {
      case ParamRepr.ConstPath(parseIdx, path, _) =>
        s"Param.ConstPath(parseIdx = $parseIdx, ${path.map(_.unesc).mkString("/")})"
      case self: ParamRepr.FunctionArg[?] =>
        s"Param.${self.productPrefix}(parseIdx = ${self.parseIdx}, paramIdx = ${self.paramIdx}, ${self.name}[${self.typeRepr.showAnsiCode}])"
    }

  def withParseIndex(idx: Int): ParamRepr[T]

}
object ParamRepr {

  final class WithCodec[T](val paramRepr: ParamRepr[T])(val codecExpr: Expr[RequestCodec[T]])

  sealed trait FunctionArg[T] extends ParamRepr[T] {

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

  sealed trait PathLike[T] extends ParamRepr[T] {
    override def makeCodec(using Quotes): Expr[RequestCodec.PathLike[T]]
  }

  final case class ConstPath(
      parseIdx: Int,
      paths: NonEmptyList[String],
      tpe: Type[Unit],
  ) extends PathLike[Unit] {

    override def makeCodec(using Quotes): Expr[RequestCodec.PathLike[Unit]] =
      '{ RequestCodec.path.const(${ Expr(paths.toList) }) }

    override def withParseIndex(idx: Int): ParamRepr[Unit] = copy(parseIdx = idx)

  }

  final case class AppliedPath[T](
      valDef: ValDef,
      name: String,
      doc: Option[String],
      parseIdx: Int,
      paramIdx: Int,
      baseCodec: Expr[PartialPathCodec[T]],
      tpe: Type[T],
  ) extends ParamRepr.PathLike[T],
        ParamRepr.FunctionArg[T] {

    override def makeCodec(using Quotes): Expr[RequestCodec.PathLike[T]] =
      '{ RequestCodec.path.fromPartial($baseCodec, ${ Expr(name) }, ${ Expr(doc) }) }

    override def withParseIndex(idx: Int): ParamRepr[T] = copy(parseIdx = idx)

  }

  final case class CustomPath[T](
      valDef: ValDef,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[RequestCodec.PathLike[T]],
      tpe: Type[T],
  ) extends PathLike[T],
        ParamRepr.FunctionArg[T] {

    val name: String = valDef.name

    override def makeCodec(using Quotes): Expr[RequestCodec.PathLike[T]] = codec

    override def withParseIndex(idx: Int): ParamRepr[T] = copy(parseIdx = idx)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      NonPath
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NonPathLike[T] extends ParamRepr.FunctionArg[T] {
    override def makeCodec(using Quotes): Expr[RequestCodec.NonPathLike[T]]
  }

  final case class AppliedMethod(
      valDef: ValDef,
      name: String,
      doc: Option[String],
      parseIdx: Int,
      paramIdx: Int,
      tpe: Type[zio.http.Method],
  ) extends NonPathLike[zio.http.Method] {

    override def makeCodec(using Quotes): Expr[RequestCodec.NonPathLike[zio.http.Method]] =
      '{ RequestCodec.anyMethod(${ Expr(name) }, ${ Expr(doc) }) }

    override def withParseIndex(idx: Int): ParamRepr[zio.http.Method] = copy(parseIdx = idx)

  }

  final case class AppliedQueryParam[T](
      valDef: ValDef,
      name: String,
      doc: Option[String],
      parseIdx: Int,
      paramIdx: Int,
      baseCodec: Expr[PartialParamCodec[T]],
      tpe: Type[T],
  ) extends NonPathLike[T] {

    override def makeCodec(using Quotes): Expr[RequestCodec.NonPathLike[T]] =
      '{ RequestCodec.query.fromPartial($baseCodec, ${ Expr(name) }, ${ Expr(doc) }) }

    override def withParseIndex(idx: Int): ParamRepr[T] = copy(parseIdx = idx)

  }

  final case class AppliedHeader[T](
      valDef: ValDef,
      name: String,
      doc: Option[String],
      parseIdx: Int,
      paramIdx: Int,
      baseCodec: Expr[PartialParamCodec[T]],
      tpe: Type[T],
  ) extends NonPathLike[T] {

    override def makeCodec(using Quotes): Expr[RequestCodec.NonPathLike[T]] =
      '{ RequestCodec.header.fromPartial($baseCodec, ${ Expr(name) }, ${ Expr(doc) }) }

    override def withParseIndex(idx: Int): ParamRepr[T] = copy(parseIdx = idx)

  }

  final case class AppliedBody[T](
      valDef: ValDef,
      name: String,
      doc: Option[String],
      parseIdx: Int,
      paramIdx: Int,
      baseCodec: Expr[PartialBodyCodec[T]],
      tpe: Type[T],
  ) extends NonPathLike[T] {

    override def makeCodec(using Quotes): Expr[RequestCodec.NonPathLike[T]] =
      '{ RequestCodec.body.fromPartial($baseCodec, ${ Expr(name) }, ${ Expr(doc) }) }

    override def withParseIndex(idx: Int): ParamRepr[T] = copy(parseIdx = idx)

  }

  final case class CustomNonPath[T](
      valDef: ValDef,
      parseIdx: Int,
      paramIdx: Int,
      codec: Expr[RequestCodec.NonPathLike[T]],
      tpe: Type[T],
  ) extends NonPathLike[T] {

    val name: String = valDef.name

    override def makeCodec(using Quotes): Expr[RequestCodec.NonPathLike[T]] = codec

    override def withParseIndex(idx: Int): ParamRepr[T] = copy(parseIdx = idx)

  }

}
