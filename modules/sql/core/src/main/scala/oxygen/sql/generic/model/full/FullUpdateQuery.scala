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

sealed trait FullUpdateQuery {

  val inputs: List[InputPart]
  val ret: ReturningPart
  val refs: RefMap

  def show(using Quotes): String

  protected[model] def allQueryRefs: Growable[VariableReference]

  def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term]

}
object FullUpdateQuery {

  final case class Basic(
      inputs: List[InputPart],
      update: UpdatePart,
      joins: List[JoinPart],
      where: Option[WherePart],
      set: SetPart,
      ret: ReturningPart.Basic,
      refs: RefMap,
  ) extends FullUpdateQuery {

    override def show(using Quotes): String =
      s"""${showInputs(inputs)}
         |UPDATE ${update.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |${set.show}$showReturning
         |""".stripMargin

    override protected[model] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        Growable(update.queryRefOrPlaceholder),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.filterExpr.queryRefs),
        Growable.many(set.setExprs.toList).flatMap(p => p.fieldToSetExpr.queryRef +: p.setValueExpr.queryRefs),
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        updateFrag <- fragmentBuilder.update(update)
        setFrag <- fragmentBuilder.set(set)
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        returningFrag <- fragmentBuilder.ret(ret, "              ")
        frag = GeneratedFragment.of(
          updateFrag,
          setFrag,
          whereFrag,
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
        expr = makeQuery(queryName, QueryContext.QueryType.Update, update.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  def wrap(inputs: List[InputPart], u: PartialQuery.UpdateQuery, ret: ReturningPart.NonSubQuery, refs: RefMap)(using ParseContext): ParseResult[FullUpdateQuery] =
    ret match {
      case ret: ReturningPart.Basic =>
        ParseResult.success { FullUpdateQuery.Basic(inputs, u.update, u.joins, u.where, u.set, ret, refs) }
      case ret: ReturningPart.Aggregate =>
        ParseResult.error(ret.fullTree, "aggregate queries not allowed as update returning")
    }

}
