package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.full.FullSelectQuery
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.Q
import scala.quoted.*

sealed trait ReturningPart {

  val fullTree: Tree
  val returningExprs: List[ReturningPart.Elem]

  def showOpt(using Quotes): Option[String]

}
object ReturningPart {

  sealed trait Elem {

    def queryRefs: Growable[VariableReference]

  }
  object Elem {

    final case class SubQuery(
        expr: QueryExpr.QueryVariableReferenceLike.ReferencedVariable,
        as: String,
    ) extends Elem {

      override def queryRefs: Growable[VariableReference] = expr.queryRefs

    }

    sealed trait NonSubQuery extends Elem {

      final def show(using Quotes): String = this match
        case Basic(expr)                                      => expr.show
        case Aggregate.ReturnFromSelf(expr)                   => s"Aggregate.Basic : ${expr.show}"
        case Aggregate.ReturnLeafAgg(select, _, _, outType)   => s"\nAggregate.Leaf(${outType.showShortCode}) {\n  ${select.show.replaceAll("\n", "\n  ")}\n}".replaceAll("\n", "\n       ")
        case Aggregate.ReturnNestedAgg(select, _, _, outType) => s"\nAggregate.Nested(${outType.showShortCode}) {\n  ${select.show.replaceAll("\n", "\n  ")}\n}".replaceAll("\n", "\n       ")

    }
    object NonSubQuery extends Parser[(Term, RefMap), ReturningPart.Elem.NonSubQuery] {

      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ReturningPart.Elem.NonSubQuery] = input match
        case Aggregate.optional(elem) => elem
        case Basic.required(elem)     => elem

    }

    final case class Basic(expr: QueryExpr) extends Elem.NonSubQuery {

      override def queryRefs: Growable[VariableReference] = expr.queryRefs

    }
    object Basic extends Parser[(Term, RefMap), ReturningPart.Elem.Basic] {

      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ReturningPart.Elem.Basic] =
        for {
          expr <- RawQueryExpr.parse(input)
          expr <- QueryExpr.parse(expr)
        } yield Elem.Basic(expr)

    }

    sealed trait Aggregate extends Elem.NonSubQuery
    object Aggregate extends Parser[(Term, RefMap), ReturningPart.Elem.Aggregate] {

      sealed trait NonNested extends Aggregate

      final case class ReturnFromSelf(expr: QueryExpr) extends Elem.Aggregate.NonNested {
        override def queryRefs: Growable[VariableReference] = expr.queryRefs
      }

      sealed trait NonBasic extends Aggregate {

        val select: FullSelectQuery.NonSubQuery
        val aggType: ReturningPart.Aggregate.AggType
        val aType: TypeRepr
        val outType: TypeRepr

        override final def queryRefs: Growable[VariableReference] = select.allQueryRefs

      }

      final case class ReturnLeafAgg(
          select: FullSelectQuery.Basic,
          aggType: ReturningPart.Aggregate.AggType,
          aType: TypeRepr,
          outType: TypeRepr,
      ) extends Elem.Aggregate.NonBasic,
            Elem.Aggregate.NonNested

      final case class ReturnNestedAgg(
          select: FullSelectQuery.Aggregate,
          aggType: ReturningPart.Aggregate.AggType,
          aType: TypeRepr,
          outType: TypeRepr,
      ) extends Elem.Aggregate.NonBasic

      // workaround to bypass silly compiler not wanting to allow extracting   '{ Q.agg.many[s] }
      private type HKT[F[x]] = F

      private def parse1(term: Term)(using ParseContext, Quotes): ParseResult[(ReturningPart.Aggregate.AggType, TypeRepr, TypeRepr, Term)] = {
        term.asExpr match {
          case '{ Q.agg.many[HKT[s]](using $seqExpr).apply[a]($subExpr) } =>
            type MyS[x]
            given mySType: Type[MyS] = TypeRepr.of[s].asTypeOf
            ParseResult.success((ReturningPart.Aggregate.AggType.Many(mySType, mySType.toTypeRepr, seqExpr.asExprOf[SeqOps[MyS]]), TypeRepr.of[a], TypeRepr.of[MyS[a]], subExpr.toTerm))
          case '{ Q.agg.manyNonEmpty.apply[a]($subExpr) } => ParseResult.success((ReturningPart.Aggregate.AggType.ManyNonEmpty, TypeRepr.of[a], TypeRepr.of[NonEmptyList[a]], subExpr.toTerm))
          case '{ Q.agg.optional.apply[a]($subExpr) }     => ParseResult.success((ReturningPart.Aggregate.AggType.Optional, TypeRepr.of[a], TypeRepr.of[Option[a]], subExpr.toTerm))
          case '{ Q.agg.required.apply[a]($subExpr) }     => ParseResult.success((ReturningPart.Aggregate.AggType.Required, TypeRepr.of[a], TypeRepr.of[a], subExpr.toTerm))
          case _                                          => ParseResult.unknown(term, "not a ReturningPart.Elem.Aggregate")
        }
      }

      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ReturningPart.Elem.Aggregate] = {
        val (term, refs) = input

        for {
          (aggType, aType, outType, subQueryTerm) <- parse1(term)
          select <- FullSelectQuery.NonSubQuery.parse((subQueryTerm, refs)).unknownAsError
          agg = select match {
            case select: FullSelectQuery.Basic     => Aggregate.ReturnLeafAgg(select, aggType, aType, outType)
            case select: FullSelectQuery.Aggregate => Aggregate.ReturnNestedAgg(select, aggType, aType, outType)
          }
        } yield agg
      }

    }

  }

  sealed trait NonEmpty extends ReturningPart {
    val returningExprsNel: NonEmptyList[Elem]
  }

  sealed trait NonSubQuery extends ReturningPart {
    override val returningExprs: List[Elem.NonSubQuery]
  }
  object NonSubQuery extends Parser[(Term, RefMap), ReturningPart.NonSubQuery] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ReturningPart.NonSubQuery] =
      for {
        (term, refs) <- ParseResult.Success(input)
        terms = term match
          case unitExpr()       => Nil
          case tupleApply(args) => args.toList
          case _                => term :: Nil

        elems <- terms.traverse { t => ParseContext.add("ReturningPart.Elem") { Elem.NonSubQuery.parse((t, refs)).unknownAsError: ParseResult[ReturningPart.Elem.NonSubQuery] } }

        nonSubQuery <- NonEmptyList.fromList(elems) match {
          case Some(elems) =>
            def allBasic: Option[NonEmptyList[Elem.Basic]] =
              elems.traverse {
                case elem: Elem.Basic  => elem.some
                case _: Elem.Aggregate => None
              }
            def allAgg: ParseResult[NonEmptyList[Elem.Aggregate]] =
              elems.traverse {
                case elem: Elem.Basic     => ParseResult.success(Elem.Aggregate.ReturnFromSelf(elem.expr))
                case elem: Elem.Aggregate => ParseResult.success(elem)
              }

            // FIX-PRE-MERGE (KR) : collect into nested/non-nested
            allBasic match {
              case Some(elems) => ParseResult.success(ReturningPart.BasicNel(term, elems))
              case None        => allAgg.map(ReturningPart.Aggregate(term, _))
            }
          case None => ParseResult.success(ReturningPart.BasicUnit(term))
        }
      } yield nonSubQuery

  }

  sealed trait Basic extends NonSubQuery {
    override val returningExprs: List[Elem.Basic]
  }
  object Basic extends Parser[(Term, RefMap), ReturningPart.Basic] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ReturningPart.Basic] =
      NonSubQuery.parse(input).flatMap {
        case ret: ReturningPart.Basic     => ParseResult.success(ret)
        case ret: ReturningPart.Aggregate => ParseResult.error(ret.fullTree, "returning aggregate not allowed")
      }

  }

  final case class BasicUnit(
      fullTree: Tree,
  ) extends Basic {

    override val returningExprs: List[Elem.Basic] = Nil

    def showOpt(using Quotes): Option[String] = returningExprs match
      case Nil         => None
      case expr :: Nil => expr.expr.show.some
      case exprs       => exprs.map(_.expr.show).mkString(", ").some

  }

  final case class BasicNel(
      fullTree: Tree,
      returningExprsNel: NonEmptyList[ReturningPart.Elem.Basic],
  ) extends Basic,
        NonEmpty {

    override val returningExprs: List[Elem.Basic] = returningExprsNel.toList

    def showOpt(using Quotes): Option[String] = returningExprs match
      case Nil         => None
      case expr :: Nil => expr.expr.show.some
      case exprs       => exprs.map(_.expr.show).mkString(", ").some

  }

  sealed trait Aggregate extends NonSubQuery, NonEmpty {

    val fullTree: Tree
    val returningExprsNel: NonEmptyList[ReturningPart.Elem.Aggregate]

    final def showOpt(using Quotes): Option[String] = returningExprs match
      case Nil         => None
      case expr :: Nil => expr.show.some
      case exprs       => exprs.map(_.show).mkString(", ").some

  }
  object Aggregate {

    final case class NonNested(
        fullTree: Tree,
        returningExprsNel: NonEmptyList[ReturningPart.Elem.Aggregate.NonNested],
    ) extends NonSubQuery,
          NonEmpty {

      override val returningExprs: List[Elem.NonSubQuery] = returningExprsNel.toList

    }

    final case class Nested(
        fullTree: Tree,
        returningExprsNel: NonEmptyList[ReturningPart.Elem.Aggregate],
    ) extends NonSubQuery,
          NonEmpty {

      override val returningExprs: List[Elem.NonSubQuery] = returningExprsNel.toList

    }

    enum AggType {
      case Required
      case Optional
      case Many[S[_]](sType: Type[S], sTypeRepr: TypeRepr, seqOpsExpr: Expr[SeqOps[S]])
      case ManyNonEmpty

      final def show: String = this match
        case AggType.Required              => "Required"
        case AggType.Optional              => "Optional"
        case AggType.Many(_, sTypeRepr, _) => s"Many[${sTypeRepr.showAnsiCode}]"
        case AggType.ManyNonEmpty          => "ManyNonEmpty"

    }

  }

  final case class SubQuery(
      fullTree: Tree,
      returningExprsNel: NonEmptyList[ReturningPart.Elem.SubQuery],
  ) extends NonEmpty {

    override val returningExprs: List[Elem.SubQuery] = returningExprsNel.toList

    def showOpt(using Quotes): Option[String] = returningExprs match
      case Nil         => None
      case expr :: Nil => expr.expr.show.some
      case exprs       => exprs.map(_.expr.show).mkString(", ").some

  }

}
