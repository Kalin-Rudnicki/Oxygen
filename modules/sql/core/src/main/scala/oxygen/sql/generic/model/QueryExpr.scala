package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

private[generic] sealed trait QueryExpr {

  /**
    * Represents the full term of this [[RawQueryExpr]].
    * Whether that is a single identifier, or a combination of Exprs.
    */
  val fullTerm: Term
  def queryRefs: Growable[QueryParam]

  final def show(using Quotes): String = this match
    case QueryExpr.UnaryInput.QueryRefIdent(_, queryRef)                             => queryRef.show
    case QueryExpr.UnaryInput.ProductFieldSelect(select, inner)                      => s"${inner.show}.${select.name.cyanFg}"
    case QueryExpr.UnaryInput.SelectPrimaryKey(_, inner, _)                          => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}"
    case QueryExpr.UnaryInput.SelectNonPrimaryKey(_, inner, _)                       => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}"
    case QueryExpr.UnaryQuery.QueryRefIdent(_, QueryParam.Query(param, _, _, true))  => param.name.hexFg("#A81ADB").toString
    case QueryExpr.UnaryQuery.QueryRefIdent(_, QueryParam.Query(param, _, _, false)) => param.name.hexFg("#540D6E").toString
    case QueryExpr.UnaryQuery.ProductFieldSelect(select, inner)                      => s"${inner.show}.${select.name.cyanFg}"
    case QueryExpr.UnaryQuery.OptionGet(_, inner)                                    => s"${inner.show}.${"get".blueFg}"
    case sel @ QueryExpr.UnaryQuery.SelectPrimaryKey(_, inner, _)                    => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}(using ${sel.rowRepr.show})"
    case sel @ QueryExpr.UnaryQuery.SelectNonPrimaryKey(_, inner, _)                 => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}(using ${sel.rowRepr.show})"
    case QueryExpr.BinaryAndOr(_, lhs, op, rhs)                                      => s"${lhs.showWrapAndOr} ${op.show} ${rhs.showWrapAndOr}"
    case comp: QueryExpr.BinaryComp                                                  => s"${comp.lhs.show} ${comp.op.show} ${comp.rhs.show}"

  final def isAndOr: Boolean = this match
    case _: QueryExpr.BinaryAndOr => true
    case _                        => false

  final def showWrapAndOr(using Quotes): String = this match
    case _: QueryExpr.BinaryAndOr => s"($show)"
    case _                        => show

}
private[generic] object QueryExpr extends Parser[RawQueryExpr, QueryExpr] {

  sealed trait Unary extends QueryExpr {
    val rootIdent: Ident
    val queryRef: QueryParam
    override final def queryRefs: Growable[QueryParam] = Growable.single(queryRef)
    final lazy val param: Function.NamedParam = queryRef.param
  }
  object Unary extends Parser[RawQueryExpr.Unary, QueryExpr.Unary] {

    override def parse(expr: RawQueryExpr.Unary)(using ParseContext, Quotes): ParseResult[QueryExpr.Unary] =
      expr match {
        case RawQueryExpr.QueryRefIdent(term, queryRef: QueryParam.Query) =>
          ParseResult.Success(QueryExpr.UnaryQuery.QueryRefIdent(term, queryRef))
        case RawQueryExpr.QueryRefIdent(term, queryRef: QueryParam.InputLike) =>
          ParseResult.Success(QueryExpr.UnaryInput.QueryRefIdent(term, queryRef))
        case RawQueryExpr.ProductField(select, inner) =>
          parse(inner).flatMap {
            case inner: QueryExpr.UnaryQuery.CanSelect    => ParseResult.Success(QueryExpr.UnaryQuery.ProductFieldSelect(select, inner))
            case inner: QueryExpr.UnaryInput.CanSelect    => ParseResult.Success(QueryExpr.UnaryInput.ProductFieldSelect(select, inner))
            case inner: QueryExpr.UnaryQuery.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.UnaryInput.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
        case RawQueryExpr.OptionGet(select, inner) =>
          parse(inner).flatMap {
            case inner: QueryExpr.UnaryQuery.CanSelect    => ParseResult.Success(QueryExpr.UnaryQuery.OptionGet(select, inner))
            case inner: QueryExpr.UnaryQuery.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case _: QueryExpr.UnaryInput                  => ParseResult.error(select, "not supported: `Option.get` on input")
          }
        case RawQueryExpr.SelectPrimaryKey(apply, inner, givenTableRepr) =>
          parse(inner).flatMap {
            case inner: QueryExpr.UnaryQuery.CanSelect    => ParseResult.Success(QueryExpr.UnaryQuery.SelectPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.UnaryInput.CanSelect    => ParseResult.Success(QueryExpr.UnaryInput.SelectPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.UnaryQuery.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.UnaryInput.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
        case RawQueryExpr.SelectNonPrimaryKey(apply, inner, givenTableRepr) =>
          parse(inner).flatMap {
            case inner: QueryExpr.UnaryQuery.CanSelect    => ParseResult.Success(QueryExpr.UnaryQuery.SelectNonPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.UnaryInput.CanSelect    => ParseResult.Success(QueryExpr.UnaryInput.SelectNonPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.UnaryQuery.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.UnaryInput.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
      }

  }

  sealed trait UnaryInput extends Unary, TermTransformer {
    def inTpe: TypeRepr
    def outTpe: TypeRepr
    override val queryRef: QueryParam.InputLike
  }
  object UnaryInput {

    sealed trait CanSelect extends UnaryInput
    sealed trait CanNotSelect extends UnaryInput

    final case class QueryRefIdent(ident: Ident, queryRef: QueryParam.InputLike) extends CanSelect, TermTransformer.Defer {
      override val fullTerm: Term = ident
      override val rootIdent: Ident = ident
      override val inTpe: TypeRepr = queryRef.param.tpe.widen
      override val outTpe: TypeRepr = queryRef.param.tpe.widen
      override protected def defer: TermTransformer = queryRef
    }

    final case class ProductFieldSelect(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryParam.InputLike = inner.queryRef
      override val inTpe: TypeRepr = inner.inTpe
      override val outTpe: TypeRepr = select.tpe.widen
      override def toRoot: TermTransformer.Root = inner >>> TermTransformer.FromSelect(select)
    }

    final case class SelectPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryParam.InputLike = inner.queryRef

      private object doApply extends TermTransformer.Transform {
        override val inTpe: TypeRepr = inner.inTpe
        override val outTpe: TypeRepr = apply.tpe.widen
        override protected def convertTermInternal(term: Term)(using Quotes): Term = givenTableRepr.getPK(term)
      }

      override val inTpe: TypeRepr = doApply.inTpe
      override val outTpe: TypeRepr = doApply.outTpe

      override def toRoot: TermTransformer.Root = inner >>> doApply
    }

    final case class SelectNonPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryParam.InputLike = inner.queryRef

      private object doApply extends TermTransformer.Transform {
        override val inTpe: TypeRepr = inner.inTpe
        override val outTpe: TypeRepr = apply.tpe.widen
        override protected def convertTermInternal(term: Term)(using Quotes): Term = givenTableRepr.getNPK(term)
      }

      override val inTpe: TypeRepr = doApply.inTpe
      override val outTpe: TypeRepr = doApply.outTpe

      override def toRoot: TermTransformer.Root = inner >>> doApply
    }

  }

  sealed trait UnaryQuery extends Unary {
    def rowRepr(using Quotes): TypeclassExpr.RowRepr
    override val queryRef: QueryParam.Query
    final lazy val isRoot: Boolean = queryRef.isRoot
  }
  object UnaryQuery {

    sealed trait CanSelect extends UnaryQuery
    sealed trait CanNotSelect extends UnaryQuery

    final case class QueryRefIdent(ident: Ident, queryRef: QueryParam.Query) extends CanSelect {
      override val fullTerm: Term = ident
      override val rootIdent: Ident = ident
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = queryRef.rowRepr
    }

    final case class ProductFieldSelect(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryParam.Query = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = inner.rowRepr.productSchemaField(select, select.symbol.name)
    }

    final case class OptionGet(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryParam.Query = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = inner.rowRepr.optionSchemaGet(select)
    }

    final case class SelectPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryParam.Query = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = givenTableRepr.pkRowRepr(fullTerm)
    }

    final case class SelectNonPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: QueryParam.Query = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = givenTableRepr.npkRowRepr(fullTerm)
    }

  }

  sealed trait Binary extends QueryExpr {
    val lhs: QueryExpr
    val rhs: QueryExpr
    override final def queryRefs: Growable[QueryParam] = lhs.queryRefs ++ rhs.queryRefs
  }

  final case class BinaryAndOr(fullTerm: Term, lhs: QueryExpr, op: BinOp.AndOr, rhs: QueryExpr) extends Binary

  sealed trait BinaryComp extends Binary {
    val lhs: Unary
    val op: BinOp.Comp
    val rhs: Unary
  }
  object BinaryComp {
    final case class QueryQuery(fullTerm: Term, lhs: UnaryQuery, op: BinOp.Comp, rhs: UnaryQuery) extends BinaryComp
    final case class QueryInput(fullTerm: Term, lhs: UnaryQuery, op: BinOp.Comp, rhs: UnaryInput) extends BinaryComp
    final case class InputQuery(fullTerm: Term, lhs: UnaryInput, op: BinOp.Comp, rhs: UnaryQuery) extends BinaryComp
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
            case (lhs: UnaryQuery, rhs: UnaryQuery) => ParseResult.Success(BinaryComp.QueryQuery(term, lhs, op, rhs))
            case (lhs: UnaryQuery, rhs: UnaryInput) => ParseResult.Success(BinaryComp.QueryInput(term, lhs, op, rhs))
            case (lhs: UnaryInput, rhs: UnaryQuery) => ParseResult.Success(BinaryComp.InputQuery(term, lhs, op, rhs))
            case (_: UnaryInput, _: UnaryInput)     => ParseResult.error(term, "can not compare 2 inputs")
          }
        } yield expr
      case RawQueryExpr.Binary(term, lhs, op: BinOp.AndOr, rhs) =>
        for {
          lhs <- QueryExpr.parse(lhs)
          rhs <- QueryExpr.parse(rhs)
        } yield QueryExpr.BinaryAndOr(term, lhs, op, rhs)
    }

}
