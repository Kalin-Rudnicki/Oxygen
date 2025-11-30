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
    case VariableReference.InputParam(param)                            => param.name.greenFg.toString
    case VariableReference.OptionalInputParam(param)                    => s"optional(${param.name.greenFg})"
    case VariableReference.ConstInputParam(param, term, _)              => s"const(${param.name.greenFg} = ${term.showAnsiCode})"
    case varRef @ VariableReference.QueryTableReference(_, _, _, true)  => varRef.sqlString.hexFg("#7EB77F").toString
    case varRef @ VariableReference.QueryTableReference(_, _, _, false) => varRef.sqlString.hexFg("#FFADC6").toString
    case varRef: VariableReference.SubQueryReference                    => varRef.sqlString.hexFg("#FF7DC6").toString

}
private[generic] object VariableReference {

  /////// Input ///////////////////////////////////////////////////////////////

  sealed trait InputLike extends VariableReference, TermTransformer {
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

  final case class ConstInputParam(
      param: Function.NamedParam,
      term: Term,
      anyTpe: TypeRepr,
  ) extends InputLike,
        TermTransformer.Die

  /////// Query ///////////////////////////////////////////////////////////////

  sealed trait QueryLike extends VariableReference {
    protected val param: Function.NamedParam
    val tableRepr: TypeclassExpr.TableRepr
    val rowRepr: TypeclassExpr.RowRepr
    val isRoot: Boolean
    val sqlString: String
  }

  final case class QueryTableReference(
      protected val param: Function.NamedParam,
      tableRepr: TypeclassExpr.TableRepr,
      rowRepr: TypeclassExpr.RowRepr,
      isRoot: Boolean,
  ) extends QueryLike {
    override val sqlString: String = param.name.camelToSnake
  }
  object QueryTableReference {

    def apply(param: Function.NamedParam, tableRepr: TypeclassExpr.TableRepr, isRoot: Boolean): QueryTableReference =
      QueryTableReference(param, tableRepr, tableRepr.tableRowRepr, isRoot)

  }

  final case class SubQueryReference(
      param: Function.NamedParam,
      tableRepr: TypeclassExpr.TableRepr,
      rowRepr: TypeclassExpr.RowRepr,
      isRoot: Boolean,
      subQueryTableName: String,
      retAs: String,
  ) extends QueryLike {
    override val sqlString: String = s"($subQueryTableName.$retAs)"
  }

}
