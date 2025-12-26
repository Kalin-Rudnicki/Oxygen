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

    final case class Aggregate(
        aggType: AggType,
        select: AggregateSelectPart,
        aType: TypeRepr,
        outType: TypeRepr,
    ) extends Elem.NonSubQuery
    object Aggregate extends Parser[(Term, RefMap), ReturningPart.Elem.Aggregate] {

      // workaround to bypass silly compiler not wanting to allow extracting   '{ Q.agg.many[s] }
      private type HKT[F[x]] = F

      private def parseAggregateCore(term: Term)(using ParseContext, Quotes): ParseResult[(AggType, TypeRepr, TypeRepr, Term)] = {
        term.asExpr match {
          case '{ Q.agg.many[HKT[s]](using $seqExpr).apply[a]($subExpr) } =>
            type MyS[x]
            given mySType: Type[MyS] = TypeRepr.of[s].asTypeOf
            ParseResult.success((AggType.Many(mySType, mySType.toTypeRepr, seqExpr.asExprOf[SeqOps[MyS]]), TypeRepr.of[a], TypeRepr.of[MyS[a]], subExpr.toTerm))
          case '{ Q.agg.manyNonEmpty.apply[a]($subExpr) } => ParseResult.success((AggType.ManyNonEmpty, TypeRepr.of[a], TypeRepr.of[NonEmptyList[a]], subExpr.toTerm))
          case '{ Q.agg.optional.apply[a]($subExpr) }     => ParseResult.success((AggType.Optional, TypeRepr.of[a], TypeRepr.of[Option[a]], subExpr.toTerm))
          case '{ Q.agg.required.apply[a]($subExpr) }     => ParseResult.success((AggType.Required, TypeRepr.of[a], TypeRepr.of[a], subExpr.toTerm))
          case _                                          => ParseResult.unknown(term, "not a ReturningPart.Elem.Aggregate")
        }
      }

      @scala.annotation.nowarn // FIX-PRE-MERGE (KR) : remove
      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ReturningPart.Elem.Aggregate] = {
        val (term, refs) = input

        // FIX-PRE-MERGE (KR) : query-agg-bookmark
        for {
          (aggType, aType, outType, subQueryTerm) <- parseAggregateCore(term)
          /*
          select <- FullSelectQuery.NonSubQuery.parse((subQueryTerm, refs)).unknownAsError
          agg = select match {
            case select: FullSelectQuery.Basic     => Aggregate.ReturnLeafAgg(select, aggType, aType, outType)
            case select: FullSelectQuery.Aggregate => Aggregate.ReturnNestedAgg(select, aggType, aType, outType)
          }
        } yield agg
           */
        } yield ???
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

    extension (elems: NonEmptyList[Elem.NonSubQuery])
      private def allBasic: Option[NonEmptyList[Elem.Basic]] =
        elems.traverse {
          case elem: Elem.Basic  => elem.some
          case _: Elem.Aggregate => None
        }

    extension (elems: NonEmptyList[Elem.NonSubQuery])
      private def allAgg(using ParseContext): ParseResult[NonEmptyList[Elem.Aggregate]] =
        elems.traverse {
          case Elem.Basic(expr: QueryExpr.QueryVariableReferenceLike.ReferencedVariable) => ParseResult.success(Elem.Aggregate.ReturnFromSelf(expr))
          case Elem.Basic(expr)                                                          => ParseResult.error(expr.fullTerm, "Not yet supported, nested aggregate queries can only return full rows")
          case elem: Elem.Aggregate                                                      => ParseResult.success(elem)
        }

    extension (elems: NonEmptyList[Elem.Aggregate])
      private def allNonNested: Option[NonEmptyList[Elem.Aggregate.NonNested]] =
        elems.traverse {
          case elem: Elem.Aggregate.NonNested    => elem.some
          case _: Elem.Aggregate.ReturnNestedAgg => None
        }

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
            elems.allBasic match {
              case Some(allBasic) => ParseResult.success(ReturningPart.BasicNel(term, allBasic))
              case None           =>
                elems.allAgg.map { elems =>
                  elems.allNonNested match {
                    case Some(allNonNested) => ReturningPart.Aggregate.NonNested(term, allNonNested)
                    case None               => ReturningPart.Aggregate.Nested(term, elems)
                  }
                }
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

  final case class Aggregate(
      fullTree: Tree,
      returningExprsNel: NonEmptyList[ReturningPart.Elem.NonSubQuery],
  ) extends NonSubQuery, NonEmpty {

    def showOpt(using Quotes): Option[String] = returningExprs match
      case Nil         => None
      case expr :: Nil => expr.show.some
      case exprs       => exprs.map(_.show).mkString(", ").some

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
