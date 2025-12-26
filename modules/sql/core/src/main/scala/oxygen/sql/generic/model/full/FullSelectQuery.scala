package oxygen.sql.generic.model.full

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.generation.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.full.Util.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.*
import scala.quoted.*

sealed trait FullSelectQuery {

  val inputs: List[InputPart]
  val ret: ReturningPart
  val refs: RefMap

  def show(using Quotes): String

  protected[model] def allQueryRefs: Growable[VariableReference]

  def makeFragment(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment]

  def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term]

}
object FullSelectQuery {

  sealed trait NonSubQuery extends FullSelectQuery {

    val ret: ReturningPart.NonSubQuery

  }
  object NonSubQuery {

    def wrap(inputs: List[InputPart], s: PartialQuery.SelectQuery, ret: ReturningPart.NonSubQuery, refs: RefMap)(using ParseContext): ParseResult[FullSelectQuery.NonSubQuery] =
      ret match {
        case ret: ReturningPart.BasicNel =>
          ParseResult.success { FullSelectQuery.Basic(inputs, s.select, s.joins, s.where, s.orderBy, s.limit, s.offset, ret, refs) }
        case ret: ReturningPart.Aggregate =>
          FullSelectQuery.Aggregate.wrap(inputs, s, ret, refs)
        case ret: ReturningPart.BasicUnit =>
          ParseResult.error(ret.fullTree, "select can not return unit")
      }

  }

  final case class Basic(
      inputs: List[InputPart],
      select: SelectPart,
      joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      ret: ReturningPart.BasicNel,
      refs: RefMap,
  ) extends FullSelectQuery.NonSubQuery {

    override def show(using Quotes): String =
      s"""${showInputs(inputs)}
         |SELECT ${ret.showOpt.getOrElse("<ERROR>")}
         |    ${select.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}${orderBy.map(_.show).mkString}${limit.map(_.show).mkString}${offset.map(_.show).mkString}
         |""".stripMargin

    override protected[model] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        select match {
          case select: SelectPart.FromTable    => Growable(select.mapQueryRef)
          case select: SelectPart.FromSubQuery => Growable.many(select.mapQueryRefs)
        },
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.filterExpr.queryRefs),
        Growable.option(orderBy).flatMap(p => Growable.many(p.orderByExprs).map(_.queryExpr.queryRef)),
        Growable.option(limit).flatMap(_.limitQueryExpr.queryRefs),
        Growable.option(offset).flatMap(_.offsetQueryExpr.queryRefs),
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def makeFragment(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
      val fragmentBuilder = FragmentBuilder(inputs)
      for {
        returningFrag <- fragmentBuilder.requiredRet(ret, "       ")
        selectFrag <- select match
          case select: SelectPart.FromTable    => fragmentBuilder.select(select)
          case select: SelectPart.FromSubQuery => fragmentBuilder.select(select)
        joinFrag <- joins.traverse(fragmentBuilder.join).map(GeneratedFragment.flatten(_))
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        orderByFrag <- orderBy.traverse(fragmentBuilder.orderBy).map(GeneratedFragment.option)
        limitFrag <- limit.traverse(fragmentBuilder.limit).map(GeneratedFragment.option)
        offsetFrag <- offset.traverse(fragmentBuilder.offset).map(GeneratedFragment.option)
      } yield GeneratedFragment.of(
        "SELECT ",
        returningFrag,
        selectFrag,
        joinFrag,
        whereFrag,
        orderByFrag,
        limitFrag,
        offsetFrag,
      )
    }

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        frag <- makeFragment
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr
        _ <- ParseResult.validate(builtDecoder.nonEmpty)(ret.fullTree, "expected non-empty return")

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Select, select.optTableRepr, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }
  object Basic extends Parser[(Term, RefMap), FullSelectQuery.Basic] {

    def wrap(inputs: List[InputPart], s: PartialQuery.SelectQuery, ret: ReturningPart.NonSubQuery, refs: RefMap)(using ParseContext): ParseResult[FullSelectQuery.Basic] =
      NonSubQuery.wrap(inputs, s, ret, refs).flatMap {
        case basic: Basic => ParseResult.success(basic)
        case _: Aggregate => ParseResult.error(ret.fullTree, "Aggregate returning not allowed here")
      }

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[FullSelectQuery.Basic] =
      PartialQuery.SelectQuery.fullParserAcceptingRefs.parse(input).flatMap { //
        case FullQueryResult(inputs, select: PartialQuery.SelectQuery, ret, refs) => wrap(inputs, select, ret, refs)
      }

  }

  trait Aggregate extends FullSelectQuery.NonSubQuery {
    // FIX-PRE-MERGE (KR) :
  }
  object Aggregate {

    def wrap(inputs: List[InputPart], s: PartialQuery.SelectQuery, ret: ReturningPart.Aggregate, refs: RefMap)(using ParseContext): ParseResult[FullSelectQuery.Aggregate] =
      ??? // FIX-PRE-MERGE (KR) :

  }

  final case class SubQuery(
      inputs: List[InputPart], // FIX-PRE-MERGE (KR) : remove?
      select: SelectPart,
      joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      ret: ReturningPart.SubQuery,
      refs: RefMap,
  ) extends FullSelectQuery {

    override def show(using Quotes): String =
      s"""${showInputs(inputs)}
         |SELECT ${ret.showOpt.getOrElse("<ERROR>")}
         |    ${select.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}${orderBy.map(_.show).mkString}${limit.map(_.show).mkString}${offset.map(_.show).mkString}
         |""".stripMargin

    override protected[model] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        select match {
          case select: SelectPart.FromTable    => Growable(select.mapQueryRef)
          case select: SelectPart.FromSubQuery => Growable.many(select.mapQueryRefs)
        },
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.filterExpr.queryRefs),
        Growable.option(orderBy).flatMap(p => Growable.many(p.orderByExprs).map(_.queryExpr.queryRef)),
        Growable.option(limit).flatMap(_.limitQueryExpr.queryRefs),
        Growable.option(offset).flatMap(_.offsetQueryExpr.queryRefs),
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def makeFragment(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
      val fragmentBuilder = FragmentBuilder(inputs)
      for {
        returningFrag <- fragmentBuilder.requiredRet(ret, "       ")
        selectFrag <- select match
          case select: SelectPart.FromTable    => fragmentBuilder.select(select)
          case select: SelectPart.FromSubQuery => fragmentBuilder.select(select)
        joinFrag <- joins.traverse(fragmentBuilder.join).map(GeneratedFragment.flatten(_))
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        orderByFrag <- orderBy.traverse(fragmentBuilder.orderBy).map(GeneratedFragment.option)
        limitFrag <- limit.traverse(fragmentBuilder.limit).map(GeneratedFragment.option)
        offsetFrag <- offset.traverse(fragmentBuilder.offset).map(GeneratedFragment.option)
      } yield GeneratedFragment.of(
        "SELECT ",
        returningFrag,
        selectFrag,
        joinFrag,
        whereFrag,
        orderByFrag,
        limitFrag,
        offsetFrag,
      )
    }

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] = {
      // FIX-PRE-MERGE (KR) :
      /*
      for {
        frag <- makeFragment
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr
        _ <- ParseResult.validate(builtDecoder.nonEmpty)(ret.fullTree, "expected non-empty return")

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Select, select.optTableRepr, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm
       */

      report.errorAndAbort("internal defect : `FullSelectQuery.SubQuery.toTerm` should never be called...")
    }

  }

}
