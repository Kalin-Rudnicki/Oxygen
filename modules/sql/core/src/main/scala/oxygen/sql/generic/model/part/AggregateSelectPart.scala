package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

sealed trait AggregateSelectPart {

  def queryRefs: Growable[VariableReference] = ??? // FIX-PRE-MERGE (KR) :

}
object AggregateSelectPart extends Parser[(Term, RefMap), AggregateSelectPart] {

  final case class ReturningLeaf(
      // TODO (KR) : are inputs needed, or a separate type?
      //           : inputs: List[InputPart],
      select: SelectPart,
      // TODO (KR) : maybe support joins?
      //           : joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      retExpr: QueryExpr.QueryVariableReferenceLike.ReferencedVariable,
      refs: RefMap,
  ) extends AggregateSelectPart

  final case class ReturningNested(
      // TODO (KR) : are inputs needed, or a separate type?
      //           : inputs: List[InputPart],
      select: SelectPart,
      // TODO (KR) : maybe support joins?
      //           : joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      ret: NonEmptyList[ReturningPart.Elem.Aggregate],
      refs: RefMap,
  ) extends AggregateSelectPart

  private final case class Partial(
      select: SelectPart,
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
  )
  private object Partial {

    lazy val partialParser: MapChainParser[AggregateSelectPart.Partial] =
      (
        SelectPart.withContext("Select") >>>
          WherePart.maybe.withContext("Where") >>>
          OrderByPart.maybe.withContext("OrderBy") >>>
          LimitPart.maybe.withContext("Limit") >>>
          OffsetPart.maybe.withContext("Offset")
      ).withContext("Aggregate Query").map { AggregateSelectPart.Partial.apply }

  }

  override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[AggregateSelectPart] =
    ??? // FIX-PRE-MERGE (KR) :

}

// FIX-PRE-MERGE (KR) :
/*


  final case class Aggregate(
      inputs: List[InputPart],
      select: SelectPart,
      joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      ret: ReturningPart.Aggregate,
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
        Growable.many(ret.returningExprs).flatMap(_.queryRefs),
      ).flatten

    override def makeFragment(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
      /*
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
 */

      ??? // FIX-PRE-MERGE (KR) :
    }

    // FIX-PRE-MERGE (KR) : remove
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
    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] = {

      val retAgg = RetAgg.from(this)

      // types:
      //   - FullSelectQuery.Basic
      //   - FullSelectQuery.Aggregate
      //     - ReturningPart.Elem.Aggregate.Basic
      //     - ReturningPart.Elem.Aggregate.Basic
      //     - ReturningPart.Elem.Aggregate.Nested

      report.errorAndAbort(
        s"""
           |=====| TypeHunt |=====
           |
           |${retAgg.toGroupedString}
           |
           |${retAgg.toIndentedString.toStringColorized}
           |""".stripMargin,
      )

      report.errorAndAbort(this.show)
      ??? // FIX-PRE-MERGE (KR) :
    }

  }

 */
