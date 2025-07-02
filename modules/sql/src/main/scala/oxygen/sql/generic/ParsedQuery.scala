package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.schema.*
import scala.quoted.*

private[sql] sealed trait ParsedQuery extends Product {

  val inputs: List[QueryParser.Input]
  val ret: QueryParser.Returning
  val refs: RefMap

  def show(using Quotes): String

  protected final def showInputs: String =
    if (inputs.nonEmpty) inputs.map(_.show).mkString("inputs:", "", "\n")
    else "<no inputs>\n"

  protected final def showReturning(using Quotes): String =
    ret.showOpt.map { ret => s"\n    RETURNING $ret" }.mkString

  protected def allQueryRefs: Growable[QueryReference]

  // TODO (KR) : probably move this out separately

  private lazy val inputParams: List[QueryReference.InputParam] = inputs.map(_.queryRef).collect { case p: QueryReference.InputParam => p }

  @scala.annotation.nowarn // FIX-PRE-MERGE (KR) : remove
  private final def makeEncoderElem(expr: QueryExpr.InputLike, encoder: Option[Expr[InputEncoder[?]]])(using ParseContext, Quotes): ParseResult[Expr[InputEncoder[?]]] =
    ParseResult.error(expr.fullTerm, "TODO : `makeEncoderElem`")

  protected final def makeEnc(expr: QueryExpr)(using ParseContext, Quotes): ParseResult[GeneratedInputEncoder] =
    ParsedQuery.makeInputEncoder(makeEncoderElem, expr)

  def toTerm(using ParseContext, Quotes): ParseResult[Term]

}
private[sql] object ParsedQuery extends Parser[Term, ParsedQuery] {

  final case class InsertQuery(
      inputs: List[QueryParser.Input],
      insert: QueryParser.InsertQ,
      into: QueryParser.Into,
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |INSERT INTO ${insert.show}
         |    VALUES ${into.queryRef.show}$showReturning
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable(insert.queryRef, into.queryRef),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      ParseResult.error(Position.ofMacroExpansion, "TODO")

  }

  final case class SelectQuery(
      inputs: List[QueryParser.Input],
      select: QueryParser.SelectQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |SELECT ${ret.showOpt.getOrElse("<ERROR>")}
         |    ${select.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable(select.queryRef),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.where.queryRefs),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      for {
        returningEnc <- ret.exprs.traverse(makeEnc).map(GeneratedInputEncoder.seq)
        joinEnc <- joins.traverse(j => makeEnc(j.on)).map(GeneratedInputEncoder.seq)
        whereEnc <- where.traverse(w => makeEnc(w.where)).map(GeneratedInputEncoder.option)
        enc = returningEnc ++ joinEnc ++ whereEnc
        _ = report.errorAndAbort(enc.buildExpr[Any].showAnsiCode)
        _ <- ParseResult.error(Position.ofMacroExpansion, "TODO")
      } yield ???

  }

  final case class UpdateQuery(
      inputs: List[QueryParser.Input],
      update: QueryParser.UpdateQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      set: QueryParser.Set,
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |UPDATE ${update.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |${set.show}$showReturning
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable(update.queryRef),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.where.queryRefs),
        Growable.many(set.parts.toList).flatMap(p => Growable(p.lhsExpr.queryRef, p.rhsExpr.queryRef)),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      ParseResult.error(Position.ofMacroExpansion, "TODO")

  }

  final case class DeleteQuery(
      inputs: List[QueryParser.Input],
      delete: QueryParser.DeleteQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |DELETE FROM ${delete.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}$showReturning
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable(delete.queryRef),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.where.queryRefs),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      ParseResult.error(Position.ofMacroExpansion, "TODO")

  }

  private def makeInputEncoder(
      makeEncoderElem: (QueryExpr.InputLike, Option[Expr[InputEncoder[?]]]) => ParseResult[Expr[InputEncoder[?]]],
      expr: QueryExpr,
  )(using Quotes, ParseContext): ParseResult[GeneratedInputEncoder] =
    expr match {
      case expr: QueryExpr.InputLike       => makeEncoderElem(expr, None).map(GeneratedInputEncoder.single)
      case _: QueryExpr.QueryLike          => ParseResult.Success(GeneratedInputEncoder.empty)
      case QueryExpr.AndOr(_, lhs, _, rhs) =>
        for {
          lhsEncoder <- makeInputEncoder(makeEncoderElem, lhs)
          rhsEncoder <- makeInputEncoder(makeEncoderElem, rhs)
        } yield lhsEncoder ++ rhsEncoder
      case _: QueryExpr.Comp.QueryQuery              => ParseResult.Success(GeneratedInputEncoder.empty)
      case QueryExpr.Comp.QueryInput(_, lhs, _, rhs) => makeEncoderElem(rhs, '{ ${ lhs.rowRepr }.encoder }.some).map(GeneratedInputEncoder.single)
      case QueryExpr.Comp.InputQuery(_, lhs, _, rhs) => makeEncoderElem(lhs, '{ ${ rhs.rowRepr }.encoder }.some).map(GeneratedInputEncoder.single)
    }

  final class FragmentBuilder() { // TODO (KR) :

    def build(expr: QueryExpr): ParseResult[GeneratedFragment] =
      ??? // FIX-PRE-MERGE (KR) :

  }

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[ParsedQuery] =
    QueryParser.finished
      .parse(input)
      .map {
        case (inputs, QueryParser.PartialQuery.InsertQuery(insert, into), ret, refs) =>
          ParsedQuery.InsertQuery(inputs, insert, into, ret, refs)
        case (inputs, QueryParser.PartialQuery.SelectQuery(select, joins, where), ret, refs) =>
          ParsedQuery.SelectQuery(inputs, select, joins, where, ret, refs)
        case (inputs, QueryParser.PartialQuery.UpdateQuery(update, joins, where, set), ret, refs) =>
          ParsedQuery.UpdateQuery(inputs, update, joins, where, set, ret, refs)
        case (inputs, QueryParser.PartialQuery.DeleteQuery(delete, joins, where), ret, refs) =>
          ParsedQuery.DeleteQuery(inputs, delete, joins, where, ret, refs)
      }
      .map { parsed =>
        val specifiedQueryRefs: Set[QueryReference.InputLike] = parsed.refs.allQueryRefs.collect { case ref: QueryReference.InputLike => ref }.to[Set]
        val usedQueryRefs: Set[QueryReference.InputLike] = parsed.allQueryRefs.collect { case ref: QueryReference.InputLike => ref }.to[Set]
        val unusedQueryRefs: Set[QueryReference.InputLike] = specifiedQueryRefs &~ usedQueryRefs
        unusedQueryRefs.foreach { ref => report.warning("unused query input param", ref.param.tree.pos) }
        parsed
      }

  def compile(input: Term)(using Quotes): Term =
    ParseContext.root("compile") {
      for {
        parsed <- ParseContext.add("parse") { ParsedQuery.parse(input) }
        _ = report.info(parsed.show)
        res <- ParseContext.add(s"${parsed.productPrefix}.toTerm") { parsed.toTerm }
      } yield res
    }

}
