package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

private[generic] sealed trait VariableReference {

  protected val param: Function.NamedParam
  final def internalParam: Function.NamedParam = param

  final def tpe: TypeRepr = param.tpe

  final def show: String = this match
    case VariableReference.FromInput(param)                     => param.name.greenFg.toString
    case VariableReference.OptionalFromInput(param)             => s"optional(${param.name.greenFg})"
    case VariableReference.FromConstInput(param, term, _)       => s"const(${param.name.greenFg} = ${term.showAnsiCode})"
    case VariableReference.FromQuery(_, _, _, true, sqlString)  => sqlString.hexFg("#7EB77F").toString
    case VariableReference.FromQuery(_, _, _, false, sqlString) => sqlString.hexFg("#FFADC6").toString

}
private[generic] object VariableReference {

  sealed trait InputLike extends VariableReference, TermTransformer {
    val param: Function.NamedParam
  }

  sealed trait NonConstInput extends InputLike, TermTransformer.DeferToParam {
    val nonConstInputType: TypeRepr
  }

  final case class FromInput(
      param: Function.NamedParam,
  ) extends NonConstInput {
    override val nonConstInputType: TypeRepr = param.tpe
  }

  final case class OptionalFromInput(
      param: Function.NamedParam,
  ) extends NonConstInput {
    override val nonConstInputType: TypeRepr = {
      given Quotes = param.tpe.quotes
      type T
      given Type[T] = param.tpe.asTypeOf
      TypeRepr.of[Option[T]]
    }
  }

  final case class FromConstInput(
      param: Function.NamedParam,
      term: Term,
      anyTpe: TypeRepr,
  ) extends InputLike,
        TermTransformer.Die

  final case class FromQuery(
      protected val param: Function.NamedParam,
      optTableRepr: Option[TypeclassExpr.TableRepr],
      rowRepr: TypeclassExpr.RowRepr,
      isRoot: Boolean,
      sqlString: String,
  ) extends VariableReference
  object FromQuery {

    def apply(param: Function.NamedParam, tableRepr: TypeclassExpr.TableRepr, isRoot: Boolean): FromQuery =
      FromQuery(param, tableRepr.some, tableRepr.tableRowRepr, isRoot, param.name.camelToSnake)

    def apply(param: Function.NamedParam, tableRepr: TypeclassExpr.TableRepr, rowRepr: TypeclassExpr.RowRepr, isRoot: Boolean): FromQuery =
      FromQuery(param, tableRepr.some, rowRepr, isRoot, param.name.camelToSnake)

    def apply(param: Function.NamedParam, rowRepr: TypeclassExpr.RowRepr, isRoot: Boolean): FromQuery =
      FromQuery(param, None, rowRepr, isRoot, param.name.camelToSnake)

    def subquery(
        param: Function.NamedParam,
        rowRepr: TypeclassExpr.RowRepr,
        isRoot: Boolean,
        sqlString: String,
    ): FromQuery =
      FromQuery(param, None, rowRepr, isRoot, sqlString)

  }

}
