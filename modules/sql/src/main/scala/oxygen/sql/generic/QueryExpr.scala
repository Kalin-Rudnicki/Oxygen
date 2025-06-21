package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] sealed trait QueryExpr {

  val term: Term

  final def show: String = this match
    case QueryExpr.InputLike.Ref(_, param, _)                 => param.name.hexFg("#F71735").toString
    case QueryExpr.InputLike.ProductField(_, inner, field, _) => s"${inner.show}.${field.cyanFg}"
    case QueryExpr.QueryLike.Ref(_, param, _, true)           => param.name.hexFg("#A81ADB").toString
    case QueryExpr.QueryLike.Ref(_, param, _, false)          => param.name.hexFg("#540D6E").toString
    case QueryExpr.QueryLike.ProductField(_, inner, field)    => s"${inner.show}.${field.cyanFg}"
    case QueryExpr.QueryLike.OptionGet(_, inner)              => s"${inner.show}.${"get".blueFg}"
    case QueryExpr.AndOr(_, lhs, op, rhs)                     => s"${lhs.showWrapAndOr} ${op.show} ${rhs.showWrapAndOr}"
    case comp: QueryExpr.Comp                                 => s"${comp.lhs.show} ${comp.op.show} ${comp.rhs.show}"

  final def isAndOr: Boolean = this match
    case _: QueryExpr.AndOr => true
    case _                  => false

  final def showWrapAndOr: String = this match
    case _: QueryExpr.AndOr => s"($show)"
    case _                  => show

}
private[generic] object QueryExpr extends Parser[RawQueryExpr, QueryExpr] {

  sealed trait Unary extends QueryExpr {
    val param: Function.Param
  }
  object Unary extends Parser[RawQueryExpr.Unary, QueryExpr.Unary] {

    override def parse(expr: RawQueryExpr.Unary)(using ParseContext, Quotes): ParseResult[QueryExpr.Unary] =
      expr match {
        case RawQueryExpr.Ref(term, QueryReference.Query(param, schema, isRoot)) =>
          ParseResult.Success(QueryExpr.QueryLike.Ref(term, param, schema, isRoot))
        case RawQueryExpr.Ref(term, QueryReference.Input(param, idx)) =>
          ParseResult.Success(QueryExpr.InputLike.Ref(term, param, idx))
        case RawQueryExpr.ProductField(term, ref, field, access) =>
          parse(ref).map {
            case ref: QueryExpr.QueryLike => QueryExpr.QueryLike.ProductField(term, ref, field)
            case ref: QueryExpr.InputLike => QueryExpr.InputLike.ProductField(term, ref, field, access)
          }
        case RawQueryExpr.OptionGet(term, ref) =>
          parse(ref).flatMap {
            case ref: QueryExpr.QueryLike => ParseResult.Success(QueryExpr.QueryLike.OptionGet(term, ref))
            case _: QueryExpr.InputLike   => ParseResult.error(term, "not supported: `Option.get` on input")
          }
      }

  }

  sealed trait InputLike extends Unary {
    val idx: Option[Int]
    def fromInput(using Quotes): Option[Expr[Any] => Expr[Any]]
  }
  object InputLike {

    final case class Ref(term: Term, param: Function.Param, idx: Option[Int]) extends InputLike {
      override def fromInput(using Quotes): Option[Expr[Any] => Expr[Any]] = param.fromInput
    }

    final case class ProductField(term: Term, inner: InputLike, field: String, access: Expr[Any] => Expr[Any]) extends InputLike {
      override val param: Function.Param = inner.param
      override val idx: Option[Int] = inner.idx
      override def fromInput(using Quotes): Option[Expr[Any] => Expr[Any]] = inner.fromInput match
        case Some(innerF) => Some { i => access(innerF(i)) }
        case None         => access.some
    }

  }

  sealed trait QueryLike extends Unary {
    val tableSchema: Expr[TableRepr[?, ?]]
    def rowRepr(using Quotes): Expr[RowRepr[?]]
    val isRoot: Boolean
  }
  object QueryLike {

    final case class Ref(term: Term, param: Function.Param, tableSchema: Expr[TableRepr[?, ?]], isRoot: Boolean) extends QueryLike {
      def rowRepr(using Quotes): Expr[RowRepr[?]] = '{ $tableSchema.rowRepr }
    }

    final case class ProductField(term: Term, inner: QueryLike, field: String) extends QueryLike {
      override val param: Function.Param = inner.param
      override val tableSchema: Expr[TableRepr[?, ?]] = inner.tableSchema
      override val isRoot: Boolean = inner.isRoot
      def rowRepr(using Quotes): Expr[RowRepr[?]] = productSchemaField(term, field, inner.rowRepr)
    }

    final case class OptionGet(term: Term, inner: QueryLike) extends QueryLike {
      override val param: Function.Param = inner.param
      override val tableSchema: Expr[TableRepr[?, ?]] = inner.tableSchema
      override val isRoot: Boolean = inner.isRoot
      def rowRepr(using Quotes): Expr[RowRepr[?]] = optionSchemaGet(term, inner.rowRepr)
    }

  }

  sealed trait Binary extends QueryExpr

  final case class AndOr(term: Term, lhs: QueryExpr, op: BinOp.AndOr, rhs: QueryExpr) extends QueryExpr.Binary

  sealed trait Comp extends Binary {
    val lhs: Unary
    val op: BinOp.Comp
    val rhs: Unary
  }
  object Comp {
    final case class Case1(term: Term, lhs: QueryLike, op: BinOp.Comp, rhs: QueryLike) extends Comp
    final case class Case2(term: Term, lhs: QueryLike, op: BinOp.Comp, rhs: InputLike) extends Comp
    final case class Case3(term: Term, lhs: InputLike, op: BinOp.Comp, rhs: QueryLike) extends Comp
  }

  override def parse(expr: RawQueryExpr)(using ParseContext, Quotes): ParseResult[QueryExpr] =
    expr match {
      case expr: RawQueryExpr.Unary =>
        Unary.parse(expr)
      case RawQueryExpr.Binary(term, lhs, op: BinOp.Comp, rhs) =>
        for {
          lhs <- lhs match {
            case lhs: RawQueryExpr.Unary => ParseResult.Success(lhs)
            case _                       => ParseResult.error(term, "can only compare unary operations")
          }
          rhs <- rhs match {
            case rhs: RawQueryExpr.Unary => ParseResult.Success(rhs)
            case _                       => ParseResult.error(term, "can only compare unary operations")
          }
          lhs <- Unary.parse(lhs)
          rhs <- Unary.parse(rhs)
          expr <- (lhs, rhs) match {
            case (lhs: QueryLike, rhs: QueryLike) => ParseResult.Success(Comp.Case1(term, lhs, op, rhs))
            case (lhs: QueryLike, rhs: InputLike) => ParseResult.Success(Comp.Case2(term, lhs, op, rhs))
            case (lhs: InputLike, rhs: QueryLike) => ParseResult.Success(Comp.Case3(term, lhs, op, rhs))
            case (_: InputLike, _: InputLike)     => ParseResult.error(term, "can not compare 2 inputs")
          }
        } yield expr
      case RawQueryExpr.Binary(term, lhs, op: BinOp.AndOr, rhs) =>
        for {
          lhs <- QueryExpr.parse(lhs)
          rhs <- QueryExpr.parse(rhs)
        } yield QueryExpr.AndOr(term, lhs, op, rhs)
    }

  private def productSchemaField(term: Term, field: String, schema: Expr[RowRepr[?]])(using Quotes): Expr[RowRepr[?]] = {
    type T
    given Type[T] = term.tpe.widen.asTypeOf

    '{ $schema.unsafeChild[T](${ Expr(field) }) }
  }

  private def optionSchemaGet(term: Term, schema: Expr[RowRepr[?]])(using Quotes): Expr[RowRepr[?]] = {
    type T
    given Type[T] = term.tpe.widen.asTypeOf

    '{ $schema.unsafeRequired[T] }
  }

}
