package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.quoted.*
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] sealed trait QueryExpr {

  val term: Term

  final def show: String = this match
    case QueryExpr.InputLike.Ref(_, param)                              => param.name.hexFg("#F71735").toString
    case QueryExpr.InputLike.ProductFieldSelect(_, inner, field, _)     => s"${inner.show}.${field.name.cyanFg}"
    case QueryExpr.QueryLike.Ref(_, param, _, true)                     => param.name.hexFg("#A81ADB").toString
    case QueryExpr.QueryLike.Ref(_, param, _, false)                    => param.name.hexFg("#540D6E").toString
    case QueryExpr.QueryLike.ProductFieldSelect(_, inner, selectSym, _) => s"${inner.show}.${selectSym.name.cyanFg}"
    case QueryExpr.QueryLike.OptionGet(_, inner)                        => s"${inner.show}.${"get".blueFg}"
    case QueryExpr.AndOr(_, lhs, op, rhs)                               => s"${lhs.showWrapAndOr} ${op.show} ${rhs.showWrapAndOr}"
    case comp: QueryExpr.Comp                                           => s"${comp.lhs.show} ${comp.op.show} ${comp.rhs.show}"

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
        case RawQueryExpr.Ref(term, QueryReference.Input(param)) =>
          ParseResult.Success(QueryExpr.InputLike.Ref(term, param))
        case RawQueryExpr.ProductField(term, ref, selectSym, selectReturnType) =>
          parse(ref).map {
            case ref: QueryExpr.QueryLike => QueryExpr.QueryLike.ProductFieldSelect(term, ref, selectSym, selectReturnType)
            case ref: QueryExpr.InputLike => QueryExpr.InputLike.ProductFieldSelect(term, ref, selectSym, selectReturnType)
          }
        case RawQueryExpr.OptionGet(term, ref) =>
          parse(ref).flatMap {
            case ref: QueryExpr.QueryLike => ParseResult.Success(QueryExpr.QueryLike.OptionGet(term, ref))
            case _: QueryExpr.InputLike   => ParseResult.error(term, "not supported: `Option.get` on input")
          }
      }

  }

  sealed trait InputLike extends Unary, TermTransformer
  object InputLike {

    final case class Ref(term: Term, param: Function.Param) extends InputLike {
      override def inTpe: TypeRepr = param.inTpe
      override def outTpe: TypeRepr = param.outTpe
      override protected def convertTermInternal(term: Term)(using Quotes): Term = param.convertTerm(term)
    }

    final case class ProductFieldSelect(term: Term, inner: InputLike, selectSym: Symbol, selectReturnType: TypeRepr) extends InputLike {
      override val param: Function.Param = inner.param
      val selectName: String = selectSym.name

      override def inTpe: TypeRepr = inner.inTpe
      override def outTpe: TypeRepr = selectReturnType
      override protected def convertTermInternal(term: Term)(using Quotes): Term = term.select(selectSym)
    }

  }

  sealed trait QueryLike extends Unary {
    val tableRepr: Expr[TableRepr[?]]
    def rowRepr(using Quotes): Expr[RowRepr[?]]
    val isRoot: Boolean
  }
  object QueryLike {

    final case class Ref(term: Term, param: Function.Param, tableRepr: Expr[TableRepr[?]], isRoot: Boolean) extends QueryLike {
      def rowRepr(using Quotes): Expr[RowRepr[?]] = '{ $tableRepr.rowRepr }
    }

    final case class ProductFieldSelect(term: Term, inner: QueryLike, selectSym: Symbol, selectReturnType: TypeRepr) extends QueryLike {
      override val param: Function.Param = inner.param
      override val tableRepr: Expr[TableRepr[?]] = inner.tableRepr
      override val isRoot: Boolean = inner.isRoot
      def rowRepr(using Quotes): Expr[RowRepr[?]] = productSchemaField(term, selectSym.name, inner.rowRepr)
    }

    final case class OptionGet(term: Term, inner: QueryLike) extends QueryLike {
      override val param: Function.Param = inner.param
      override val tableRepr: Expr[TableRepr[?]] = inner.tableRepr
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
