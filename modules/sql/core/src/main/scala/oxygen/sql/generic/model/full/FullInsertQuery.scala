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

sealed trait FullInsertQuery {

  val inputs: List[InputPart]
  val ret: ReturningPart
  val refs: RefMap

  def show(using Quotes): String

  protected[model] def allQueryRefs: Growable[VariableReference]

  def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term]

}
object FullInsertQuery {

  final case class Basic(
      inputs: List[InputPart],
      insert: InsertPart.Basic,
      into: IntoPart,
      ret: ReturningPart.Basic,
      refs: RefMap,
  ) extends FullInsertQuery {

    override def show(using Quotes): String =
      s"""${showInputs(inputs)}
         |INSERT INTO ${insert.show}
         |    VALUES ${into.queryExpr.show}$showReturning
         |""".stripMargin

    override protected[model] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        Growable.option(insert.mapQueryRef),
        into.queryExpr.queryRefs,
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        insertFrag <- fragmentBuilder.insert(insert)
        valuesFrag <- fragmentBuilder.values(insert, into)
        returningFrag <- fragmentBuilder.ret(ret, "              ")
        frag = GeneratedFragment.of(
          insertFrag,
          "\n    (",
          '{ ${ insert.tableRepr.expr }.rowRepr.columns.`a, b, c` },
          ")",
          valuesFrag,
          GeneratedFragment.option(
            returningFrag.map { ret =>
              GeneratedFragment.of(
                "\n    RETURNING ",
                ret,
              )
            },
          ),
        )
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Insert, insert.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  final case class FromSelect(
      inputs: List[InputPart],
      insert: InsertPart.FromSelect,
      select: FullSelectQuery.Basic,
      ret: ReturningPart.Basic,
      refs: RefMap,
  ) extends FullInsertQuery {

    override def show(using Quotes): String =
      s"""${showInputs(inputs)}
         |INSERT INTO ${insert.show}
         |${select.show}$showReturning
         |""".stripMargin

    override protected[model] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        Growable.option(insert.mapQueryRef),
        select.allQueryRefs,
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        insertFrag <- fragmentBuilder.insert(insert)
        selectFrag <- select.makeFragment
        returningFrag <- fragmentBuilder.ret(ret, "              ")
        frag = GeneratedFragment.of(
          insertFrag,
          "\n    (",
          '{ ${ insert.tableRepr.expr }.rowRepr.columns.`a, b, c` },
          ")\n",
          selectFrag,
          GeneratedFragment.option(
            returningFrag.map { ret =>
              GeneratedFragment.of(
                "\n    RETURNING ",
                ret,
              )
            },
          ),
        )
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Insert, insert.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  def wrap(inputs: List[InputPart], i: PartialQuery.InsertQuery, ret: ReturningPart.NonSubQuery, refs: RefMap)(using ParseContext): ParseResult[FullInsertQuery] =
    (i, ret) match {
      case (PartialQuery.InsertQuery.Basic(insert, into), ret: ReturningPart.Basic) =>
        ParseResult.success { FullInsertQuery.Basic(inputs, insert, into, ret, refs) }
      case (PartialQuery.InsertQuery.FromSelect(insert, s, into), ret: ReturningPart.Basic) =>
        val fullSelect = FullSelectQuery.Basic(inputs, s.select, s.joins, s.where, s.orderBy, s.limit, s.offset, into.toReturning, refs)
        ParseResult.success { FullInsertQuery.FromSelect(inputs, insert, fullSelect, ret, refs) }
      case (_, ret: ReturningPart.Aggregate) =>
        ParseResult.error(ret.fullTree, "aggregate queries not allowed as insert returning")
    }

}
