package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] sealed trait QueryExpr {

  /**
    * Represents the full term of this [[RawQueryExpr]].
    * Whether that is a single identifier, or a combination of Exprs.
    */
  val fullTerm: Term
  def queryRefs: Growable[QueryReference]

  final def show(using Quotes): String = this match
    case QueryExpr.InputLike.QueryRefIdent(_, queryRef)                              => queryRef.show
    case QueryExpr.InputLike.ProductFieldSelect(select, inner)                       => s"${inner.show}.${select.name.cyanFg}"
    case QueryExpr.InputLike.SelectPrimaryKey(_, inner, _)                           => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}"
    case QueryExpr.InputLike.SelectNonPrimaryKey(_, inner, _)                        => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}"
    case QueryExpr.QueryLike.QueryRefIdent(_, QueryReference.Query(param, _, true))  => param.name.hexFg("#A81ADB").toString
    case QueryExpr.QueryLike.QueryRefIdent(_, QueryReference.Query(param, _, false)) => param.name.hexFg("#540D6E").toString
    case QueryExpr.QueryLike.ProductFieldSelect(select, inner)                       => s"${inner.show}.${select.name.cyanFg}"
    case QueryExpr.QueryLike.OptionGet(_, inner)                                     => s"${inner.show}.${"get".blueFg}"
    case sel @ QueryExpr.QueryLike.SelectPrimaryKey(_, inner, _)                     => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}(using ${sel.rowRepr.showAnsiCode})"
    case sel @ QueryExpr.QueryLike.SelectNonPrimaryKey(_, inner, _)                  => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}(using ${sel.rowRepr.showAnsiCode})"
    case QueryExpr.AndOr(_, lhs, op, rhs)                                            => s"${lhs.showWrapAndOr} ${op.show} ${rhs.showWrapAndOr}"
    case comp: QueryExpr.Comp                                                        => s"${comp.lhs.show} ${comp.op.show} ${comp.rhs.show}"

  final def isAndOr: Boolean = this match
    case _: QueryExpr.AndOr => true
    case _                  => false

  final def showWrapAndOr(using Quotes): String = this match
    case _: QueryExpr.AndOr => s"($show)"
    case _                  => show

}
private[generic] object QueryExpr extends Parser[RawQueryExpr, QueryExpr] {

  sealed trait Unary extends QueryExpr {
    val rootIdent: Ident
    val queryRef: QueryReference
    override final def queryRefs: Growable[QueryReference] = Growable.single(queryRef)
    final lazy val param: Function.Param = queryRef.param
  }
  object Unary extends Parser[RawQueryExpr.Unary, QueryExpr.Unary] {

    override def parse(expr: RawQueryExpr.Unary)(using ParseContext, Quotes): ParseResult[QueryExpr.Unary] =
      expr match {
        case RawQueryExpr.QueryRefIdent(term, queryRef: QueryReference.Query) =>
          ParseResult.Success(QueryExpr.QueryLike.QueryRefIdent(term, queryRef))
        case RawQueryExpr.QueryRefIdent(term, queryRef: QueryReference.InputLike) =>
          ParseResult.Success(QueryExpr.InputLike.QueryRefIdent(term, queryRef))
        case RawQueryExpr.ProductField(select, inner) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryLike.CanSelect    => ParseResult.Success(QueryExpr.QueryLike.ProductFieldSelect(select, inner))
            case inner: QueryExpr.InputLike.CanSelect    => ParseResult.Success(QueryExpr.InputLike.ProductFieldSelect(select, inner))
            case inner: QueryExpr.QueryLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.InputLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
        case RawQueryExpr.OptionGet(select, inner) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryLike.CanSelect    => ParseResult.Success(QueryExpr.QueryLike.OptionGet(select, inner))
            case inner: QueryExpr.QueryLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case _: QueryExpr.InputLike                  => ParseResult.error(select, "not supported: `Option.get` on input")
          }
        case RawQueryExpr.SelectPrimaryKey(apply, inner, givenTableRepr) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryLike.CanSelect    => ParseResult.Success(QueryExpr.QueryLike.SelectPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.InputLike.CanSelect    => ParseResult.Success(QueryExpr.InputLike.SelectPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.QueryLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.InputLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
        case RawQueryExpr.SelectNonPrimaryKey(apply, inner, givenTableRepr) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryLike.CanSelect    => ParseResult.Success(QueryExpr.QueryLike.SelectNonPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.InputLike.CanSelect    => ParseResult.Success(QueryExpr.InputLike.SelectNonPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.QueryLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.InputLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
      }

  }

  sealed trait InputLike extends Unary, TermTransformer {
    def inTpe: TypeRepr
    def outTpe: TypeRepr
    override val queryRef: QueryReference.InputLike
  }
  object InputLike {

    sealed trait CanSelect extends InputLike
    sealed trait CanNotSelect extends InputLike

    final case class QueryRefIdent(ident: Ident, queryRef: QueryReference.InputLike) extends CanSelect, TermTransformer.Defer {
      override val fullTerm: Term = ident
      override val rootIdent: Ident = ident
      override val inTpe: TypeRepr = queryRef.param.tpe.widen
      override val outTpe: TypeRepr = queryRef.param.tpe.widen
      override protected def defer: TermTransformer = queryRef
    }

    final case class ProductFieldSelect(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryReference.InputLike = inner.queryRef
      override val inTpe: TypeRepr = inner.inTpe
      override val outTpe: TypeRepr = select.tpe.widen
      override def toRoot: TermTransformer.Root = inner >>> TermTransformer.FromSelect(select)
    }

    final case class SelectPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: Term) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryReference.InputLike = inner.queryRef

      private object doApply extends TermTransformer.Transform {
        override val inTpe: TypeRepr = inner.inTpe
        override val outTpe: TypeRepr = apply.tpe.widen
        override protected def convertTermInternal(term: Term)(using Quotes): Term = givenTableRepr.select("pk").select("get").appliedTo(term)
      }

      override val inTpe: TypeRepr = doApply.inTpe
      override val outTpe: TypeRepr = doApply.outTpe

      override def toRoot: TermTransformer.Root = inner >>> doApply
    }

    final case class SelectNonPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: Term) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryReference.InputLike = inner.queryRef

      private object doApply extends TermTransformer.Transform {
        override val inTpe: TypeRepr = inner.inTpe
        override val outTpe: TypeRepr = apply.tpe.widen
        override protected def convertTermInternal(term: Term)(using Quotes): Term = givenTableRepr.select("npk").select("get").appliedTo(term)
      }

      override val inTpe: TypeRepr = doApply.inTpe
      override val outTpe: TypeRepr = doApply.outTpe

      override def toRoot: TermTransformer.Root = inner >>> doApply
    }

  }

  sealed trait QueryLike extends Unary {
    def rowRepr(using Quotes): Expr[RowRepr[?]]
    override val queryRef: QueryReference.Query
    final lazy val isRoot: Boolean = queryRef.isRoot
  }
  object QueryLike {

    sealed trait CanSelect extends QueryLike
    sealed trait CanNotSelect extends QueryLike

    final case class QueryRefIdent(ident: Ident, queryRef: QueryReference.Query) extends CanSelect {
      override val fullTerm: Term = ident
      override val rootIdent: Ident = ident
      override def rowRepr(using Quotes): Expr[RowRepr[?]] = '{ ${ queryRef.tableRepr }.rowRepr }
    }

    final case class ProductFieldSelect(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryReference.Query = inner.queryRef
      override def rowRepr(using Quotes): Expr[RowRepr[?]] = productSchemaField(select, select.symbol.name, inner.rowRepr)
    }

    final case class OptionGet(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryReference.Query = inner.queryRef
      override def rowRepr(using Quotes): Expr[RowRepr[?]] = optionSchemaGet(select, inner.rowRepr)
    }

    final case class SelectPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: Term) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryReference.Query = inner.queryRef
      override def rowRepr(using Quotes): Expr[RowRepr[?]] = selectPrimaryKey(fullTerm, givenTableRepr)
    }

    final case class SelectNonPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: Term) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryReference.Query = inner.queryRef
      override def rowRepr(using Quotes): Expr[RowRepr[?]] = selectNonPrimaryKey(fullTerm, givenTableRepr)
    }

  }

  sealed trait Binary extends QueryExpr {
    val lhs: QueryExpr
    val rhs: QueryExpr
    override final def queryRefs: Growable[QueryReference] = lhs.queryRefs ++ rhs.queryRefs
  }

  final case class AndOr(fullTerm: Term, lhs: QueryExpr, op: BinOp.AndOr, rhs: QueryExpr) extends Binary

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

  private def selectPrimaryKey(fullTerm: Term, givenTableRepr: Term)(using Quotes): Expr[RowRepr[?]] = {
    type T
    given Type[T] = fullTerm.tpe.asTypeOf

    givenTableRepr.select("pk").select("rowRepr").asExprOf[RowRepr[T]]
  }

  private def selectNonPrimaryKey(fullTerm: Term, givenTableRepr: Term)(using Quotes): Expr[RowRepr[?]] = {
    type T
    given Type[T] = fullTerm.tpe.asTypeOf

    givenTableRepr.select("npk").select("rowRepr").asExprOf[RowRepr[T]]
  }

  private def optionSchemaGet(term: Term, rowRepr: Expr[RowRepr[?]])(using Quotes): Expr[RowRepr[?]] = {
    type T
    given Type[T] = term.tpe.widen.asTypeOf

    '{ $rowRepr.unsafeRequired[T] }
  }

}
