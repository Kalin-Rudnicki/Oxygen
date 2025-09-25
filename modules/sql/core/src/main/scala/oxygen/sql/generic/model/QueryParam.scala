package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.quoted.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

private[generic] sealed trait QueryParam {

  val param: Function.NamedParam

  final def show: String = this match
    case QueryParam.InputParam(param)          => param.name.greenFg.toString
    case QueryParam.ConstInput(param, term, _) => s"const(${param.name.greenFg} = ${term.showAnsiCode})"
    case QueryParam.Query(param, _, _, true)   => param.name.hexFg("#7EB77F").toString
    case QueryParam.Query(param, _, _, false)  => param.name.hexFg("#FFADC6").toString

}
private[generic] object QueryParam {

  sealed trait InputLike extends QueryParam, TermTransformer {
    val param: Function.NamedParam
  }

  final case class InputParam(
      param: Function.NamedParam,
  ) extends InputLike,
        TermTransformer.DeferToParam

  final case class ConstInput(
      param: Function.NamedParam,
      term: Term,
      anyTpe: TypeRepr,
  ) extends InputLike,
        TermTransformer.Die

  final case class Query(
      param: Function.NamedParam,
      tableRepr: TypeclassExpr.TableRepr,
      mapRowRepr: Quotes => TypeclassExpr.RowRepr => TypeclassExpr.RowRepr,
      isRoot: Boolean,
  ) extends QueryParam {

    def rowRepr(using quotes: Quotes): TypeclassExpr.RowRepr = mapRowRepr(quotes)(tableRepr.tableRowRepr)

  }
  object Query {

    def apply(param: Function.NamedParam, tableRepr: TypeclassExpr.TableRepr, isRoot: Boolean): Query =
      Query(param, tableRepr, _ => identity, isRoot)

    def mapRowRepr(param: Function.NamedParam, tableRepr: TypeclassExpr.TableRepr, f: Quotes ?=> TypeclassExpr.RowRepr => TypeclassExpr.RowRepr, isRoot: Boolean): Query =
      Query(param, tableRepr, q => f(using q), isRoot)

  }

}
