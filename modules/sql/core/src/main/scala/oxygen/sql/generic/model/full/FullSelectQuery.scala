package oxygen.sql.generic.model.full

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.generation.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.full.Util.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.*
import oxygen.zio.SparseStreamAggregator
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
  object NonSubQuery extends Parser[(Term, RefMap), FullSelectQuery.NonSubQuery] {

    def wrap(inputs: List[InputPart], s: PartialQuery.SelectQuery, ret: ReturningPart.NonSubQuery, refs: RefMap)(using ParseContext): ParseResult[FullSelectQuery.NonSubQuery] =
      ret match {
        case ret: ReturningPart.BasicNel =>
          ParseResult.success { FullSelectQuery.Basic(inputs, s.select, s.joins, s.where, s.orderBy, s.limit, s.offset, ret, refs) }
        case ret: ReturningPart.Aggregate =>
          ParseResult.success { FullSelectQuery.Aggregate(inputs, s.select, s.joins, s.where, s.orderBy, s.limit, s.offset, ret, refs) }
        case ret: ReturningPart.BasicUnit =>
          ParseResult.error(ret.fullTree, "select can not return unit")
      }

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[FullSelectQuery.NonSubQuery] =
      PartialQuery.SelectQuery.fullParserAcceptingRefs.parse(input).flatMap { //
        case FullQueryResult(inputs, select: PartialQuery.SelectQuery, ret, refs) => wrap(inputs, select, ret, refs)
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
      val fragmentBuilder = FragmentBuilder(inputs)
      for {
        // 1. Build Base Query Fragment
        // We select "*" from the base tables/subquery to ensure all columns (including correlation keys) are available
        selectFrag <- select match
          case select: SelectPart.FromTable    => fragmentBuilder.select(select)
          case select: SelectPart.FromSubQuery => fragmentBuilder.select(select)
        joinFrag <- joins.traverse(fragmentBuilder.join).map(GeneratedFragment.flatten(_))
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        orderByFrag <- orderBy.traverse(fragmentBuilder.orderBy).map(GeneratedFragment.option)
        limitFrag <- limit.traverse(fragmentBuilder.limit).map(GeneratedFragment.option)
        offsetFrag <- offset.traverse(fragmentBuilder.offset).map(GeneratedFragment.option)

        baseQueryFrag = GeneratedFragment.of(
          "SELECT ",
          selectFrag,
          joinFrag,
          whereFrag,
          orderByFrag,
          limitFrag,
          offsetFrag,
        )

        // 2. Build Lateral Part (UNION ALL of Self + Children)
        lateralFrag <- AggregateGen.makeLateralFragment(ret)

      } yield GeneratedFragment.of(
        "SELECT lat.* FROM (",
        GeneratedFragment.indented(baseQueryFrag, "  "),
        ") base CROSS JOIN LATERAL (",
        GeneratedFragment.indented(lateralFrag, "  "),
        ") lat",
      )
    }

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] = {
      for {
        frag <- makeFragment
        (builtSql, builtEncoder) = frag.buildExpr

        // Output Decoder
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr
        _ <- ParseResult.validate(builtDecoder.nonEmpty)(ret.fullTree, "expected non-empty return")

        // Aggregator
        aggExpr = AggregateGen.makeAggregator(ret)

        // Make Simple Query (which returns sparse rows)
        simpleQuery = makeQuery(queryName, QueryContext.QueryType.Select, select.optTableRepr, debug)(builtSql, builtEncoder, builtDecoder)

      } yield {
        val rowType = builtDecoder.tpe.asType
        val inputType = builtEncoder.tpe.asType

        (rowType, inputType) match {
          case ('[row], '[input]) =>
            val typedAgg = aggExpr.asExprOf[SparseStreamAggregator[row, ?]]

            if (builtEncoder.hasNonConstParams) {
              val typedQuery = simpleQuery.asExprOf[QueryIO[input, row]]
              '{ $typedQuery >>> $typedAgg }.toTerm
            } else {
              val typedQuery = simpleQuery.asExprOf[QueryO[row]]
              '{ $typedQuery >>> $typedAgg }.toTerm
            }
        }
      }
    }

  }

  object AggregateGen {
    import ReturningPart.Elem

    case class Slot(
        isSelf: Boolean,
        width: Int,
        // For Self: the expressions
        selfExprs: List[QueryExpr] = Nil,
        // For Child: the query
        childQuery: Option[FullSelectQuery] = None,
    )

    def getSlots(ret: ReturningPart.Aggregate)(using Quotes): List[Slot] = {
      ret.returningExprsNel.toList.map {
        case Elem.Aggregate.ReturnFromSelf(expr) =>
          Slot(isSelf = true, width = 1, selfExprs = List(expr))

        case Elem.Aggregate.ReturnLeafAgg(select, _, _, _) =>
          // For a Leaf Agg (Basic), the width is the number of returning expressions
          val w = select.ret.returningExprs.length
          Slot(isSelf = false, width = w, childQuery = Some(select))

        case Elem.Aggregate.ReturnNestedAgg(select, _, _, _) =>
          // For a Nested Agg, the width is the sum of widths of its slots
          val w = getWidth(select.ret)
          Slot(isSelf = false, width = w, childQuery = Some(select))
      }
    }

    def getWidth(ret: ReturningPart.Aggregate)(using Quotes): Int =
      getSlots(ret).map(_.width).sum

    def makeLateralFragment(ret: ReturningPart.Aggregate)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
      val slots = getSlots(ret)
      val totalWidth = slots.map(_.width).sum

      // 1. Self Branch
      val selfSlots = slots.filter(_.isSelf)
      val hasSelf = selfSlots.nonEmpty

      val selfBranch: Option[GeneratedFragment] =
        if hasSelf then
          // Construct: SELECT [exprs | NULLs]
          val exprs = slots.flatMap { slot =>
            if slot.isSelf then
              slot.selfExprs.map(e => GeneratedFragment.sql(e.show))
            else
              List.fill(slot.width)(GeneratedFragment.sql("NULL"))
          }
          // Join with comma
          val exprsList = exprs.flatMap(e => List(GeneratedFragment.sql(", "), e)).drop(1)
          val joined = GeneratedFragment.flatten(exprsList)
          Some(GeneratedFragment.of("SELECT ", joined))
        else None

      // 2. Child Branches
      // For each child slot, we generate a branch
      // SELECT [NULLs], child.*, [NULLs] FROM LATERAL (childQuery) child
      val childBranches: List[ParseResult[GeneratedFragment]] = slots.zipWithIndex.collect {
        case (slot, idx) if !slot.isSelf =>
          val child = slot.childQuery.get
          for {
            childFrag <- child.makeFragment
          } yield {
            // Prefix NULLs
            val prefixWidth = slots.take(idx).map(_.width).sum
            val suffixWidth = slots.drop(idx + 1).map(_.width).sum

            val prefix = List.fill(prefixWidth)(GeneratedFragment.sql("NULL"))
            val suffix = List.fill(suffixWidth)(GeneratedFragment.sql("NULL"))

            val center = List(GeneratedFragment.sql("q.*"))

            val all = prefix ++ center ++ suffix
            // Join with comma
            val allList = all.flatMap(e => List(GeneratedFragment.sql(", "), e)).drop(1)
            val joined = GeneratedFragment.flatten(allList)

            GeneratedFragment.of(
              "SELECT ",
              joined,
              " FROM LATERAL (",
              GeneratedFragment.indented(childFrag, "  "),
              ") q",
            )
          }
      }

      for {
        children <- childBranches.sequence
      } yield {
        val allBranches = selfBranch.toList ++ children
        val unionAll = GeneratedFragment.sql("\nUNION ALL\n")
        // Join branches with UNION ALL
        val branchesList = allBranches.flatMap(b => List(unionAll, b.wrapInParensIf(true))).drop(1)
        val joined = GeneratedFragment.flatten(branchesList)
        joined
      }
    }

    def makeAggregator(ret: ReturningPart.Aggregate)(using Quotes): Expr[SparseStreamAggregator[?, ?]] = {
      import oxygen.zio.SparseStreamAggregator

      def helper(elems: List[Elem]): Expr[SparseStreamAggregator[?, ?]] = {
        elems match {
          case Nil => report.errorAndAbort("Empty ReturningPart")

          case head :: Nil =>
            makeOne(head)

          case head :: tail =>
            val aggHead = makeOne(head)
            val aggTail = helper(tail)
            '{ $aggHead *: $aggTail }
        }
      }

      def makeOne(elem: Elem): Expr[SparseStreamAggregator[?, ?]] = elem match {
        case Elem.Basic(expr) =>
          expr.fullTerm.tpe.asType match {
            case '[t] => '{ SparseStreamAggregator.of[t] }
          }
        
        case Elem.Aggregate.ReturnFromSelf(expr) =>
          expr.fullTerm.tpe.asType match {
            case '[t] => '{ SparseStreamAggregator.of[t] }
          }

        case elem: Elem.Aggregate.NonBasic =>
          val innerAgg = elem.select.ret match {
            case ret: ReturningPart.Basic     => 
               helper(ret.returningExprs)
               
            case ret: ReturningPart.Aggregate => 
               helper(ret.returningExprsNel.toList)
          }
          
          elem.aggType match {
            case ReturningPart.Aggregate.AggType.Required =>
              innerAgg 

            case ReturningPart.Aggregate.AggType.Optional =>
              '{ $innerAgg.optional }

            case ReturningPart.Aggregate.AggType.Many(sType, sTypeRepr, seqOpsExpr) =>
              '{ $innerAgg.many(using $seqOpsExpr) }

            case ReturningPart.Aggregate.AggType.ManyNonEmpty =>
               '{ $innerAgg.nel }
          }
          
        case _: Elem.SubQuery =>
           report.errorAndAbort("SubQuery in Aggregate not supported yet")
      }

      helper(ret.returningExprsNel.toList)
    }

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
