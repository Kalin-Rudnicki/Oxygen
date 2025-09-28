package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

private[generic] sealed trait QueryParam {

  val param: Function.NamedParam

  final def show: String = this match
    case QueryParam.InputParam(param)          => param.name.greenFg.toString
    case QueryParam.OptionalInputParam(param)  => s"optional(${param.name.greenFg})"
    case QueryParam.ConstInput(param, term, _) => s"const(${param.name.greenFg} = ${term.showAnsiCode})"
    case QueryParam.Query(param, _, _, true)   => param.name.hexFg("#7EB77F").toString
    case QueryParam.Query(param, _, _, false)  => param.name.hexFg("#FFADC6").toString

}
private[generic] object QueryParam {

  sealed trait InputLike extends QueryParam, TermTransformer {
    val param: Function.NamedParam
  }

  sealed trait NonConstInput extends InputLike, TermTransformer.DeferToParam {
    val nonConstInputType: TypeRepr
  }

  final case class InputParam(
      param: Function.NamedParam,
  ) extends NonConstInput {
    override val nonConstInputType: TypeRepr = param.tpe
  }

  final case class OptionalInputParam(
      param: Function.NamedParam,
  ) extends NonConstInput {
    override val nonConstInputType: TypeRepr = {
      given Quotes = param.tpe.quotes
      type T
      given Type[T] = param.tpe.asTypeOf
      TypeRepr.of[Option[T]]
    }
  }

  final case class ConstInput(
      param: Function.NamedParam,
      term: Term,
      anyTpe: TypeRepr,
  ) extends InputLike,
        TermTransformer.Die

  final case class Query(
      param: Function.NamedParam,
      optTableRepr: Option[TypeclassExpr.TableRepr],
      rowRepr: TypeclassExpr.RowRepr,
      isRoot: Boolean,
  ) extends QueryParam
  object Query {

    def apply(param: Function.NamedParam, tableRepr: TypeclassExpr.TableRepr, isRoot: Boolean): Query =
      Query(param, tableRepr.some, tableRepr.tableRowRepr, isRoot)

    def apply(param: Function.NamedParam, tableRepr: TypeclassExpr.TableRepr, roweRepr: TypeclassExpr.RowRepr, isRoot: Boolean): Query =
      Query(param, tableRepr.some, roweRepr, isRoot)

    def apply(param: Function.NamedParam, roweRepr: TypeclassExpr.RowRepr, isRoot: Boolean): Query =
      Query(param, None, roweRepr, isRoot)

  }

}
