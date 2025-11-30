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

sealed trait FullDeleteQuery {

  val inputs: List[InputPart]
  val ret: ReturningPart
  val refs: RefMap

  def show(using Quotes): String

  protected[model] def allQueryRefs: Growable[VariableReference]

  def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term]

}
object FullDeleteQuery {

  final case class Basic(
      inputs: List[InputPart],
      delete: DeletePart,
      joins: List[JoinPart],
      where: Option[WherePart], // TODO (KR) : make this required?
      ret: ReturningPart.Basic,
      refs: RefMap,
  ) extends FullDeleteQuery {

    override def show(using Quotes): String =
      s"""${showInputs(inputs)}
         |DELETE FROM ${delete.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}$showReturning
         |""".stripMargin

    override protected[model] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        Growable(delete.mapQueryRef),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.filterExpr.queryRefs),
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        deleteFrag <- fragmentBuilder.delete(delete)
        joinFrag <- joins.traverse(fragmentBuilder.join).map(GeneratedFragment.flatten(_))
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        returningFrag <- fragmentBuilder.ret(ret, "              ")
        frag = GeneratedFragment.of(
          deleteFrag,
          joinFrag,
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
        expr = makeQuery(queryName, QueryContext.QueryType.Delete, delete.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  def wrap(inputs: List[InputPart], d: PartialQuery.DeleteQuery, ret: ReturningPart.NonSubQuery, refs: RefMap)(using ParseContext): ParseResult[FullDeleteQuery] =
    ret match {
      case ret: ReturningPart.Basic =>
        ParseResult.success { FullDeleteQuery.Basic(inputs, d.delete, d.joins, d.where, ret, refs) }
      case ret: ReturningPart.Aggregate =>
        ParseResult.error(ret.fullTree, "aggregate queries not allowed as update returning")
    }

}
