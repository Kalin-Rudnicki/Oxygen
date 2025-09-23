package oxygen.sql.generic.parsing

import oxygen.meta.*
import oxygen.predef.color.given
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import scala.quoted.*

private[generic] sealed trait RawQueryExpr {

  /**
    * Represents the full term of this [[RawQueryExpr]].
    * Whether that is a single identifier, or a combination of Exprs.
    */
  val fullTerm: Term

  final def isBin: Boolean = this match
    case _: RawQueryExpr.Unary  => false
    case _: RawQueryExpr.Binary => true

  final def show: String = this match {
    case RawQueryExpr.QueryRefIdent(_, ref)                         => ref.show
    case RawQueryExpr.ProductField(select, inner)                   => s"${inner.show}.${select.name.magentaFg}"
    case RawQueryExpr.OptionGet(_, inner)                           => s"${inner.show}.${"get".hexFg("#35A7FF")}"
    case RawQueryExpr.SelectPrimaryKey(_, inner, _)                 => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}"
    case RawQueryExpr.SelectNonPrimaryKey(_, inner, _)              => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}"
    case bin: RawQueryExpr.Binary if bin.lhs.isBin || bin.rhs.isBin => s"(${bin.lhs.show}) ${bin.op.show} (${bin.rhs.show})"
    case bin: RawQueryExpr.Binary                                   => s"${bin.lhs.show} ${bin.op.show} ${bin.rhs.show}"
  }

}
private[generic] object RawQueryExpr extends Parser[(Term, RefMap), RawQueryExpr] {

  sealed trait Unary extends RawQueryExpr
  object Unary extends Parser[(Term, RefMap), RawQueryExpr.Unary] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr.Unary] =
      RawQueryExpr.parse(input).flatMap {
        case unary: Unary => ParseResult.Success(unary)
        case _: Binary    => ParseResult.error(input._1, "expected unary")
      }

  }

  /**
    * An [[Ident]] which points at the given [[QueryReference]].
    * val a: ? = ???
    * `a`
    */
  final case class QueryRefIdent(ident: Ident, queryRef: QueryReference) extends RawQueryExpr.Unary {
    override val fullTerm: Term = ident
  }
  object QueryRefIdent extends Parser[(Term, RefMap), QueryRefIdent] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[QueryRefIdent] =
      input match {
        case (ident: Ident, refs) => refs.get(ident).map(QueryRefIdent(ident, _))
        case (term, _)            => ParseResult.unknown(term, "not a QueryRefIdent")
      }

  }

  /**
    * A [[Select]] on a product type.
    * final case class Person(first: String, last: String)
    * val a: Person = ???
    * `a.first`
    */
  final case class ProductField(select: Select, inner: RawQueryExpr.Unary) extends RawQueryExpr.Unary {
    override val fullTerm: Term = select
  }
  object ProductField extends Parser[(Term, RefMap), ProductField] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ProductField] =
      input match {
        case (term @ Select(lhs, _), _) if lhs.tpe <:< TypeRepr.of[Option[?]] =>
          ParseResult.unknown(term, "can not parse Option select to product select")
        case (term @ Select(lhs, funct), refs) =>
          val lhsTpe: TypeRepr = lhs.tpe.widen
          type T
          given Type[T] = lhsTpe.asTypeOf

          // TODO (KR) : consider requiring an `extends Column.Product`

          for {
            gen <-
              if (lhsTpe.typeType.product.option.nonEmpty) ParseResult.Success(K0.ProductGeneric.of[T](K0.Derivable.Config()))
              else ParseResult.unknown(lhs, s"not a product type (${lhsTpe.showAnsiCode})")
            _ <- gen.fields.iterator.find(_.name == funct) match {
              case Some(field) => ParseResult.Success(field)
              case None        => ParseResult.error(term, s"not a product field: $funct")
            }
            parsedLHS <- Unary.parse((lhs, refs))
          } yield ProductField(term, parsedLHS)
        case (term, _) =>
          ParseResult.unknown(term, "not a product select")
      }

  }

  /**
    * An Option.get.
    * val a: Option[?] = ???
    * `a.get`
    */
  final case class OptionGet(select: Select, inner: RawQueryExpr.Unary) extends RawQueryExpr.Unary {
    override val fullTerm: Term = select
  }
  object OptionGet extends Parser[(Term, RefMap), OptionGet] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[OptionGet] = {
      input match
        case (term @ Select(lhs, "get"), refs) if lhs.tpe <:< TypeRepr.of[Option[?]] => Unary.parse((lhs, refs)).map(OptionGet(term, _))
        case (term @ Select(lhs, funct), _) if lhs.tpe <:< TypeRepr.of[Option[?]]    => ParseResult.error(term, s"invalid function call on Option: $funct")
        case (term, _)                                                               => ParseResult.unknown(term, "not an option.get")
    }

  }

  final case class SelectPrimaryKey(apply: Apply, inner: RawQueryExpr.Unary, givenTableRepr: Term) extends RawQueryExpr.Unary {
    override val fullTerm: Term = apply
  }
  object SelectPrimaryKey extends Parser[(Term, RefMap), SelectPrimaryKey] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[SelectPrimaryKey] =
      input match {
        case (apply @ Apply(Apply(TypeApply(Ident("tablePK"), _ :: Nil), lhs :: Nil), givenTableRepr :: Nil), refs) => Unary.parse((lhs, refs)).map(SelectPrimaryKey(apply, _, givenTableRepr))
        case (term, _)                                                                                              => ParseResult.unknown(term, "not a primary-key select")
      }

  }

  final case class SelectNonPrimaryKey(apply: Apply, inner: RawQueryExpr.Unary, givenTableRepr: Term) extends RawQueryExpr.Unary {
    override val fullTerm: Term = apply
  }
  object SelectNonPrimaryKey extends Parser[(Term, RefMap), SelectNonPrimaryKey] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[SelectNonPrimaryKey] =
      input match {
        case (apply @ Apply(Apply(TypeApply(Ident("tableNPK"), _ :: Nil), lhs :: Nil), givenTableRepr :: Nil), refs) => Unary.parse((lhs, refs)).map(SelectNonPrimaryKey(apply, _, givenTableRepr))
        case (term, _)                                                                                               => ParseResult.unknown(term, "not a non-primary-key select")
      }

  }

  final case class Binary(fullTerm: Term, lhs: RawQueryExpr, op: BinOp, rhs: RawQueryExpr) extends RawQueryExpr
  object Binary extends Parser[(Term, RefMap), RawQueryExpr.Binary] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr.Binary] = {
      val (rootTerm, refs) = input
      rootTerm match {
        case singleApply(select @ Select(lhs, op), rhs) =>
          for {
            op <- op match
              case BinOp.Scala(op) => ParseResult.Success(op)
              case _               => ParseResult.error(select, s"invalid binary operator: $op")
            lhs <- RawQueryExpr.parse((lhs, refs))
            rhs <- RawQueryExpr.parse((rhs, refs))
          } yield Binary(rootTerm, lhs, op, rhs)
        case singleApply(singleApply(ident @ Ident(op), lhs), rhs) =>
          for {
            op <- op match
              case BinOp.Scala(op) => ParseResult.Success(op)
              case _               => ParseResult.error(ident, s"invalid binary operator: $op")
            lhs <- RawQueryExpr.parse((lhs, refs))
            rhs <- RawQueryExpr.parse((rhs, refs))
          } yield Binary(rootTerm, lhs, op, rhs)
        case _ => ParseResult.unknown(rootTerm, "unknown binary")
      }
    }

  }

  override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr] = input match
    case QueryRefIdent.optional(res)       => res
    case SelectPrimaryKey.optional(res)    => res
    case SelectNonPrimaryKey.optional(res) => res
    case OptionGet.optional(res)           => res
    case ProductField.optional(res)        => res
    case Binary.optional(res)              => res
    case (rootTerm, _)                     => ParseResult.unknown(rootTerm, "not a query expr?")

}
