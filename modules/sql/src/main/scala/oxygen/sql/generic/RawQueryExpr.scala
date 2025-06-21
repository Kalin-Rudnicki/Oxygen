package oxygen.sql.generic

import oxygen.meta.*
import oxygen.predef.color.given
import oxygen.quoted.*
import scala.quoted.*

private[generic] sealed trait RawQueryExpr {

  val term: Term

  final def isBin: Boolean = this match
    case _: RawQueryExpr.Unary  => false
    case _: RawQueryExpr.Binary => true

  final def show: String = this match {
    case RawQueryExpr.Ref(_, ref)                                   => ref.show
    case RawQueryExpr.ProductField(_, ref, field, _)                => s"${ref.show}.${field.magentaFg}"
    case RawQueryExpr.OptionGet(_, ref)                             => s"${ref.show}.${"get".hexFg("#35A7FF")}"
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

  final case class Ref(term: Term, ref: QueryReference) extends RawQueryExpr.Unary
  final case class ProductField(term: Term, ref: RawQueryExpr.Unary, field: String, access: Expr[Any] => Expr[Any]) extends RawQueryExpr.Unary
  final case class OptionGet(term: Term, ref: RawQueryExpr.Unary) extends RawQueryExpr.Unary

  final case class Binary(term: Term, lhs: RawQueryExpr, op: BinOp, rhs: RawQueryExpr) extends RawQueryExpr
  object Binary extends Parser[(Term, RefMap), RawQueryExpr.Binary] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr.Binary] =
      RawQueryExpr.parse(input).flatMap {
        case bin: Binary => ParseResult.Success(bin)
        case _: Unary    => ParseResult.error(input._1, "expected binary")
      }

  }

  private object parseProductField extends Parser[Select, (Term, String, Expr[Any] => Expr[Any])] {

    override def parse(term: Select)(using ParseContext, Quotes): ParseResult[(Term, String, Expr[Any] => Expr[Any])] =
      term match {
        case Select(lhs, funct) =>
          type T
          given Type[T] = lhs.tpe.widen.asTypeOf

          // TODO (KR) : consider requiring an `extends Column.Product`

          for {
            gen <-
              if (TypeRepr.of[T].typeTypeCase.nonEmpty) ParseResult.Success(K0.ProductGeneric.of[T](K0.Derivable.Config()))
              else ParseResult.unknown(lhs, "no product generic")
            field <- gen.fields.iterator.find(_.name == funct) match {
              case Some(field) => ParseResult.Success(field)
              case None        => ParseResult.error(term, s"not a product field: $funct")
            }
          } yield (lhs, funct, field.fromParent.asInstanceOf[Expr[Any] => Expr[Any]])
      }

  }

  private object parseOptionGet extends Parser[Select, Term] {

    override def parse(term: Select)(using ParseContext, Quotes): ParseResult[Term] = term match
      case Select(lhs, "get") if lhs.tpe <:< TypeRepr.of[Option[?]] => ParseResult.Success(lhs)
      case Select(lhs, funct) if lhs.tpe <:< TypeRepr.of[Option[?]] => ParseResult.error(term, s"invalid function call on Option: $funct")
      case _                                                        => ParseResult.unknown(term, "LHS is not an Option")

  }

  override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr] = {
    val (rootTerm, refs) = input
    rootTerm match {
      case ident: Ident =>
        refs.get(ident.symbol) match {
          case Some(ref) => ParseResult.Success(Ref(rootTerm, ref))
          case None      => ParseResult.error(ident, "not an input or query param")
        }
      case parseOptionGet.optional(res) =>
        for {
          term <- res
          expr <- Unary.parse((term, refs))
        } yield OptionGet(rootTerm, expr)
      case parseProductField.optional(res) =>
        for {
          (term, field, access) <- res
          expr <- Unary.parse((term, refs))
        } yield ProductField(rootTerm, expr, field, access)
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
      case _ =>
        ParseResult.unknown(rootTerm, "not a query expr?")
    }
  }

}
