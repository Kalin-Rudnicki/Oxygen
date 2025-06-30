package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.quoted.*
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] sealed trait QueryExpr {

  /**
    * Represents the full term of this [[RawQueryExpr]].
    * Whether that is a single identifier, or a combination of Exprs.
    */
  val fullTerm: Term

  final def show: String = this match
    case QueryExpr.InputLike.QueryRefIdent(_, queryRef)                              => queryRef.show
    case QueryExpr.InputLike.ProductFieldSelect(select, inner)                       => s"${inner.show}.${select.name.cyanFg}"
    case QueryExpr.QueryLike.QueryRefIdent(_, QueryReference.Query(param, _, true))  => param.name.hexFg("#A81ADB").toString
    case QueryExpr.QueryLike.QueryRefIdent(_, QueryReference.Query(param, _, false)) => param.name.hexFg("#540D6E").toString
    case QueryExpr.QueryLike.ProductFieldSelect(select, inner)                       => s"${inner.show}.${select.name.cyanFg}"
    case QueryExpr.QueryLike.OptionGet(_, inner)                                     => s"${inner.show}.${"get".blueFg}"
    case QueryExpr.AndOr(_, lhs, op, rhs)                                            => s"${lhs.showWrapAndOr} ${op.show} ${rhs.showWrapAndOr}"
    case comp: QueryExpr.Comp                                                        => s"${comp.lhs.show} ${comp.op.show} ${comp.rhs.show}"

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
        case RawQueryExpr.QueryRefIdent(term, queryRef: QueryReference.Query) =>
          ParseResult.Success(QueryExpr.QueryLike.QueryRefIdent(term, queryRef))
        case RawQueryExpr.QueryRefIdent(term, queryRef: QueryReference.InputLike) =>
          ParseResult.Success(QueryExpr.InputLike.QueryRefIdent(term, queryRef))
        case RawQueryExpr.ProductField(select, inner) =>
          parse(inner).map {
            case inner: QueryExpr.QueryLike => QueryExpr.QueryLike.ProductFieldSelect(select, inner)
            case inner: QueryExpr.InputLike => QueryExpr.InputLike.ProductFieldSelect(select, inner)
          }
        case RawQueryExpr.OptionGet(select, inner) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryLike => ParseResult.Success(QueryExpr.QueryLike.OptionGet(select, inner))
            case _: QueryExpr.InputLike     => ParseResult.error(select, "not supported: `Option.get` on input")
          }
      }

  }

  sealed trait InputLike extends Unary, TermTransformer
  object InputLike {

    final case class QueryRefIdent(ident: Ident, queryRef: QueryReference.InputLike) extends InputLike, TermTransformer.Defer {
      override val fullTerm: Term = ident
      override val param: Function.Param = queryRef.param
      override protected def defer: TermTransformer = queryRef
    }

    final case class ProductFieldSelect(select: Select, inner: InputLike) extends InputLike {
      override val fullTerm: Term = select
      override val param: Function.Param = inner.param
      override val inTpe: TypeRepr = inner.inTpe
      override val outTpe: TypeRepr = select.tpe.widen
      override protected def convertTermInternal(term: Term)(using Quotes): Term = term.select(select.symbol)
    }

  }

  sealed trait QueryLike extends Unary {
    val tableRepr: Expr[TableRepr[?]]
    def rowRepr(using Quotes): Expr[RowRepr[?]]
    val isRoot: Boolean
  }
  object QueryLike {

    final case class QueryRefIdent(ident: Ident, queryRef: QueryReference.Query) extends QueryLike {
      override val fullTerm: Term = ident
      override val param: Function.Param = queryRef.param
      override val tableRepr: Expr[TableRepr[?]] = queryRef.tableRepr
      override val isRoot: Boolean = queryRef.isRoot
      def rowRepr(using Quotes): Expr[RowRepr[?]] = '{ $tableRepr.rowRepr }
    }

    final case class ProductFieldSelect(select: Select, inner: QueryLike) extends QueryLike {
      override val fullTerm: Term = select
      override val param: Function.Param = inner.param
      override val tableRepr: Expr[TableRepr[?]] = inner.tableRepr
      override val isRoot: Boolean = inner.isRoot
      def rowRepr(using Quotes): Expr[RowRepr[?]] = productSchemaField(select, select.symbol.name, inner.rowRepr)
    }

    final case class OptionGet(select: Select, inner: QueryLike) extends QueryLike {
      override val fullTerm: Term = select
      override val param: Function.Param = inner.param
      override val tableRepr: Expr[TableRepr[?]] = inner.tableRepr
      override val isRoot: Boolean = inner.isRoot
      def rowRepr(using Quotes): Expr[RowRepr[?]] = optionSchemaGet(select, inner.rowRepr)
    }

  }

  sealed trait Binary extends QueryExpr

  final case class AndOr(fullTerm: Term, lhs: QueryExpr, op: BinOp.AndOr, rhs: QueryExpr) extends QueryExpr.Binary

  sealed trait Comp extends Binary {
    val lhs: Unary
    val op: BinOp.Comp
    val rhs: Unary
  }
  object Comp {
    final case class QueryQuery(fullTerm: Term, lhs: QueryLike, op: BinOp.Comp, rhs: QueryLike) extends Comp
    final case class QueryInput(fullTerm: Term, lhs: QueryLike, op: BinOp.Comp, rhs: InputLike) extends Comp
    final case class InputQuery(fullTerm: Term, lhs: InputLike, op: BinOp.Comp, rhs: QueryLike) extends Comp
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
            case (lhs: QueryLike, rhs: QueryLike) => ParseResult.Success(Comp.QueryQuery(term, lhs, op, rhs))
            case (lhs: QueryLike, rhs: InputLike) => ParseResult.Success(Comp.QueryInput(term, lhs, op, rhs))
            case (lhs: InputLike, rhs: QueryLike) => ParseResult.Success(Comp.InputQuery(term, lhs, op, rhs))
            case (_: InputLike, _: InputLike)     => ParseResult.error(term, "can not compare 2 inputs")
          }
        } yield expr
      case RawQueryExpr.Binary(term, lhs, op: BinOp.AndOr, rhs) =>
        for {
          lhs <- QueryExpr.parse(lhs)
          rhs <- QueryExpr.parse(rhs)
        } yield QueryExpr.AndOr(term, lhs, op, rhs)
    }

  private def productSchemaField(term: Term, field: String, rowRepr: Expr[RowRepr[?]])(using Quotes): Expr[RowRepr[?]] = {
    type T
    given Type[T] = term.tpe.widen.asTypeOf

    '{ $rowRepr.unsafeChild[T](${ Expr(field) }) }
  }

  private def optionSchemaGet(term: Term, rowRepr: Expr[RowRepr[?]])(using Quotes): Expr[RowRepr[?]] = {
    type T
    given Type[T] = term.tpe.widen.asTypeOf

    '{ $rowRepr.unsafeRequired[T] }
  }

}
