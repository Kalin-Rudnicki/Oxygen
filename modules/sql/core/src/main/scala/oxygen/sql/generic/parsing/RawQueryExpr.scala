package oxygen.sql.generic.parsing

import java.time.Instant
import java.util.UUID
import oxygen.meta.*
import oxygen.meta.K0.ProductGeneric
import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.query.TableCompanion
import oxygen.sql.query.dsl.Q
import oxygen.sql.schema.TableRepr
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
    case _: RawQueryExpr.Other  => false

  final def show: String = this match {
    case RawQueryExpr.QueryRefIdent(_, ref)                         => ref.show
    case RawQueryExpr.ConstValue(_, term)                           => s"{ ${term.showCode} }".cyanFg.toString
    case RawQueryExpr.StaticCount(_, out)                           => s"${"COUNT".cyanFg}( ${out.magentaFg} )"
    case RawQueryExpr.CountWithArg(_, inner)                        => s"${"COUNT".cyanFg}( ${inner.show} )"
    case RawQueryExpr.ProductField(select, inner)                   => s"${inner.show}.${select.name.magentaFg}"
    case RawQueryExpr.OptionGet(_, inner)                           => s"${inner.show}.${"get".hexFg("#35A7FF")}"
    case RawQueryExpr.SelectPrimaryKey(_, inner, _)                 => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}"
    case RawQueryExpr.SelectNonPrimaryKey(_, inner, _)              => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}"
    case bin: RawQueryExpr.Binary if bin.lhs.isBin || bin.rhs.isBin => s"(${bin.lhs.show}) ${bin.op.show} (${bin.rhs.show})"
    case bin: RawQueryExpr.Binary                                   => s"${bin.lhs.show} ${bin.op.show} ${bin.rhs.show}"
    case RawQueryExpr.InstantiateTable(_, gen, _, args)             => args.map(_.show).mkString(s"${gen.typeRepr.showCode}.apply(", ", ", ")")
    case RawQueryExpr.RandomUUID(_)                                 => "UUID.randomUUID()"
    case RawQueryExpr.InstantNow(_)                                 => "Instant.now()"
    case RawQueryExpr.OptionApply(_, inner)                         => s"Option(${inner.show})"
    case RawQueryExpr.StringConcat(_, args)                         => args.map(_.show).mkString("CONCAT(", ", ", ")")
  }

}
private[generic] object RawQueryExpr extends Parser[(Term, RefMap), RawQueryExpr] {

  sealed trait Unary extends RawQueryExpr
  object Unary extends Parser[(Term, RefMap), RawQueryExpr.Unary] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr.Unary] =
      RawQueryExpr.parse(input).flatMap {
        case unary: Unary => ParseResult.Success(unary)
        case _: Binary    => ParseResult.error(input._1, "expected unary")
        case _: Other     => ParseResult.error(input._1, "expected unary")
      }

  }

  /**
    * An [[Ident]] which points at the given [[QueryParam]].
    * val a: ? = ???
    * `a`
    */
  final case class QueryRefIdent(ident: Ident, queryParam: QueryParam) extends RawQueryExpr.Unary {
    override val fullTerm: Term = ident
  }
  object QueryRefIdent extends Parser[(Term, RefMap), QueryRefIdent] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[QueryRefIdent] =
      input match {
        case (ident: Ident, refs) => refs.get(ident).map(QueryRefIdent(ident, _))
        case (term, _)            => ParseResult.unknown(term, "not a QueryRefIdent")
      }

  }

  final case class ConstValue(fullTerm: Term, constTerm: Term) extends RawQueryExpr.Unary
  object ConstValue extends Parser[(Term, RefMap), ConstValue] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ConstValue] = {
      val (term, _) = input
      term.asExpr match
        case '{ Q.const($constExpr) } => ParseResult.Success(ConstValue(term, constExpr.toTerm))
        case _                        => ParseResult.unknown(term, "not a ConstValue")
    }

  }

  final case class StaticCount(fullTerm: Term, out: String) extends RawQueryExpr.Unary
  object StaticCount extends Parser[(Term, RefMap), StaticCount] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[StaticCount] = {
      val (term, _) = input

      term.asExpr match
        case '{ Q.count.* }  => ParseResult.Success(StaticCount(term, "*"))
        case '{ Q.count._1 } => ParseResult.Success(StaticCount(term, "1"))
        case _               => ParseResult.unknown(term, "not a static count")
    }

  }

  final case class CountWithArg(fullTerm: Term, inner: RawQueryExpr.Unary) extends RawQueryExpr.Unary
  object CountWithArg extends Parser[(Term, RefMap), CountWithArg] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[CountWithArg] = {
      val (term, refs) = input

      term.asExpr match
        case '{ Q.count { $innerExpr } } => RawQueryExpr.Unary.parse((innerExpr.toTerm, refs)).map(CountWithArg(term, _))
        case _                           => ParseResult.unknown(term, "not a count with arg")
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

  final case class SelectPrimaryKey(apply: Apply, inner: RawQueryExpr.Unary, givenTableRepr: TypeclassExpr.TableRepr) extends RawQueryExpr.Unary {
    override val fullTerm: Term = apply
  }
  object SelectPrimaryKey extends Parser[(Term, RefMap), SelectPrimaryKey] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[SelectPrimaryKey] =
      input match {
        case (apply @ Apply(Apply(TypeApply(Ident("tablePK"), _ :: Nil), lhs :: Nil), givenTableRepr :: Nil), refs) =>
          Unary.parse((lhs, refs)).map(SelectPrimaryKey(apply, _, TypeclassExpr.TableRepr.wrapTerm(givenTableRepr)))
        case (term, _) =>
          ParseResult.unknown(term, "not a primary-key select")
      }

  }

  final case class SelectNonPrimaryKey(apply: Apply, inner: RawQueryExpr.Unary, givenTableRepr: TypeclassExpr.TableRepr) extends RawQueryExpr.Unary {
    override val fullTerm: Term = apply
  }
  object SelectNonPrimaryKey extends Parser[(Term, RefMap), SelectNonPrimaryKey] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[SelectNonPrimaryKey] =
      input match {
        case (apply @ Apply(Apply(TypeApply(Ident("tableNPK"), _ :: Nil), lhs :: Nil), givenTableRepr :: Nil), refs) =>
          Unary.parse((lhs, refs)).map(SelectNonPrimaryKey(apply, _, TypeclassExpr.TableRepr.wrapTerm(givenTableRepr)))
        case (term, _) =>
          ParseResult.unknown(term, "not a non-primary-key select")
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
              case BinOp.scala(op) => ParseResult.Success(op)
              case _               => ParseResult.unknown(select, s"invalid binary operator: $op")
            lhs <- RawQueryExpr.parse((lhs, refs))
            rhs <- RawQueryExpr.parse((rhs, refs))
          } yield Binary(rootTerm, lhs, op, rhs)
        case singleApply(singleApply(ident @ Ident(op), lhs), rhs) =>
          for {
            op <- op match
              case BinOp.scala(op) => ParseResult.Success(op)
              case _               => ParseResult.unknown(ident, s"invalid binary operator: $op")
            lhs <- RawQueryExpr.parse((lhs, refs))
            rhs <- RawQueryExpr.parse((rhs, refs))
          } yield Binary(rootTerm, lhs, op, rhs)
        case _ => ParseResult.unknown(rootTerm, "unknown binary")
      }
    }

  }

  sealed trait Other extends RawQueryExpr

  final case class InstantiateTable(
      fullTerm: Term,
      gen: ProductGeneric[?],
      givenTableRepr: TypeclassExpr.TableRepr,
      args: List[RawQueryExpr],
  ) extends RawQueryExpr.Other
  object InstantiateTable extends Parser[(Term, RefMap), InstantiateTable] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr.InstantiateTable] = {
      val (rootTerm, refs) = input
      val tpe = rootTerm.tpe.widen
      type T
      given Type[T] = tpe.asTypeOf

      rootTerm match {
        case Apply(Select(lhs, "apply"), rhss) if lhs.tpe <:< TypeRepr.of[TableCompanion[?, ?]] =>
          Implicits.searchOption[TableRepr[T]] match {
            case Some(tableReprExpr) =>
              val gen = ProductGeneric.of[T]
              ParseContext.add("Table.apply") {
                rhss
                  .traverse(t => RawQueryExpr.parse((t, refs)).unknownAsError: ParseResult[RawQueryExpr])
                  .map(InstantiateTable(rootTerm, gen, TypeclassExpr.TableRepr(tableReprExpr), _))
              }
            case None =>
              ParseResult.error(rootTerm, s"no given ${TypeRepr.of[TableRepr[T]].showAnsiCode} found")
          }
        case _ =>
          ParseResult.unknown(rootTerm, "unknown table instantiation")
      }
    }

  }

  final case class RandomUUID(fullTerm: Term) extends RawQueryExpr.Other
  object RandomUUID extends Parser[(Term, RefMap), RandomUUID] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RandomUUID] = {
      val (term, _) = input

      term.asExpr match
        case '{ UUID.randomUUID() } => ParseResult.Success(RandomUUID(term))
        case '{ UUID.randomUUID }   => ParseResult.Success(RandomUUID(term))
        case _                      => ParseResult.unknown(term, "not a UUID.randomUUID")
    }

  }

  final case class InstantNow(fullTerm: Term) extends RawQueryExpr.Other
  object InstantNow extends Parser[(Term, RefMap), InstantNow] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[InstantNow] = {
      val (term, _) = input

      term.asExpr match
        case '{ Instant.now() } => ParseResult.success(InstantNow(term))
        case _                  => ParseResult.unknown(term, "not a Instant.now")
    }

  }

  final case class StringConcat(fullTerm: Term, args: List[RawQueryExpr]) extends RawQueryExpr.Other
  object StringConcat extends Parser[(Term, RefMap), StringConcat] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[StringConcat] = {
      val (term, refs) = input

      for {
        rawArgs: Seq[Expr[String]] <- term.asExpr match
          case '{ Q.mkSqlString(${ Varargs(concatArgs) }*) } => ParseResult.success(concatArgs)
          case _                                             => ParseResult.unknown(term, "not a string concat")
        parsedArgs: Seq[RawQueryExpr] <-
          ParseContext.add("Table.apply") {
            rawArgs.traverse {
              case t @ Expr(_: String) => ParseResult.success(RawQueryExpr.ConstValue(t.toTerm, t.toTerm))
              case t                   => RawQueryExpr.parse((t.toTerm, refs)).unknownAsError: ParseResult[RawQueryExpr]
            }
          }
      } yield StringConcat(term, parsedArgs.toList)
    }

  }

  final case class OptionApply(fullTerm: Term, inner: RawQueryExpr) extends RawQueryExpr.Other
  object OptionApply extends Parser[(Term, RefMap), OptionApply] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[OptionApply] = {
      val (term, refs) = input
      term.asExpr match
        case '{ Option.apply(${ inner }) } => RawQueryExpr.parse((inner.toTerm, refs)).map(OptionApply(term, _))
        case _                             => ParseResult.unknown(term, "not an Option.apply(_)")
    }

  }
  override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[RawQueryExpr] = input match
    case QueryRefIdent.optional(res)       => res
    case ConstValue.optional(res)          => res
    case StaticCount.optional(res)         => res
    case CountWithArg.optional(res)        => res
    case SelectPrimaryKey.optional(res)    => res
    case SelectNonPrimaryKey.optional(res) => res
    case OptionGet.optional(res)           => res
    case ProductField.optional(res)        => res
    case Binary.optional(res)              => res
    case InstantiateTable.optional(res)    => res
    case RandomUUID.optional(res)          => res
    case InstantNow.optional(res)          => res
    case StringConcat.optional(res)        => res
    case OptionApply.optional(res)         => res
    case (rootTerm, _)                     => ParseResult.unknown(rootTerm, "not a query expr?")

}
