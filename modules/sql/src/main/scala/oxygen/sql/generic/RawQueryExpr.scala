package oxygen.sql.generic

import oxygen.meta.*
import oxygen.predef.color.given
import oxygen.quoted.*
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
    * val a: _ = ???
    * `a`
    */
  final case class QueryRefIdent(ident: Ident, queryRef: QueryReference) extends RawQueryExpr.Unary {
    override val fullTerm: Term = ident
  }
  object QueryRefIdent extends Parser[(Ident, RefMap), QueryRefIdent] {

    override def parse(input: (Ident, RefMap))(using ParseContext, Quotes): ParseResult[QueryRefIdent] = {
      val (ident, refs) = input
      refs.get(ident.symbol) match {
        case Some(ref) => ParseResult.Success(QueryRefIdent(ident, ref))
        case None      => ParseResult.error(ident, "not an input or query param")
      }
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
  object ProductField extends Parser[(Select, RefMap), ProductField] {

    override def parse(input: (Select, RefMap))(using ParseContext, Quotes): ParseResult[ProductField] = {
      val (term, refs) = input
      val lhs = term.qualifier
      val funct = term.name
      val lhsTpe: TypeRepr = lhs.tpe.widen
      type T
      given Type[T] = lhsTpe.asTypeOf

      // TODO (KR) : consider requiring an `extends Column.Product`

      for {
        gen <-
          if (lhsTpe.typeTypeCase.nonEmpty) ParseResult.Success(K0.ProductGeneric.of[T](K0.Derivable.Config()))
          else ParseResult.unknown(lhs, s"not a product type (${lhsTpe.showAnsiCode})")
        _ <- gen.fields.iterator.find(_.name == funct) match {
          case Some(field) => ParseResult.Success(field)
          case None        => ParseResult.error(term, s"not a product field: $funct")
        }
        parsedLHS <- Unary.parse((lhs, refs))
      } yield ProductField(term, parsedLHS)
    }
  }

  final case class OptionGet(select: Select, inner: RawQueryExpr.Unary) extends RawQueryExpr.Unary {
    override val fullTerm: Term = select
  }
  object OptionGet extends Parser[(Select, RefMap), OptionGet] {

    override def parse(input: (Select, RefMap))(using ParseContext, Quotes): ParseResult[OptionGet] = {
      val (term, refs) = input
      term match
        case Select(lhs, "get") if lhs.tpe <:< TypeRepr.of[Option[?]] => Unary.parse((lhs, refs)).map(OptionGet(term, _))
        case Select(lhs, funct) if lhs.tpe <:< TypeRepr.of[Option[?]] => ParseResult.error(term, s"invalid function call on Option: $funct")
        case _                                                        => ParseResult.unknown(term, "LHS is not an Option")
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
    case QueryRefIdent.optional(res) => res
    case OptionGet.optional(res)     => res
    case ProductField.optional(res)  => res
    case Binary.optional(res)        => res
    case (rootTerm, _)               => ParseResult.unknown(rootTerm, "not a query expr?")

}
