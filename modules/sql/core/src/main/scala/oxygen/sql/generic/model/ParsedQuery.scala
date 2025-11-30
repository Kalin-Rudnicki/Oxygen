package oxygen.sql.generic.model

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.generation.*
import oxygen.sql.generic.model.full.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

private[sql] sealed trait ParsedQuery extends Product {

  val inputs: List[InputPart]
  val ret: ReturningPart
  val refs: RefMap

  def show(using Quotes): String

  protected final def showInputs: String =
    if inputs.nonEmpty then inputs.map(_.show).mkString("inputs:", "", "\n")
    else "<no inputs>\n"

  protected final def showReturning(using Quotes): String =
    ret.showOpt.map { ret => s"\n    RETURNING $ret" }.mkString

  protected[model] def allQueryRefs: Growable[VariableReference]

  def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term]

}
private[sql] object ParsedQuery extends Parser[Term, ParsedQuery] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Insert
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class InsertQuery(full: FullInsertQuery) extends ParsedQuery {

    override val inputs: List[InputPart] = full.inputs
    override val ret: ReturningPart = full.ret
    override val refs: RefMap = full.refs

    override def show(using Quotes): String = full.show

    override protected[model] def allQueryRefs: Growable[VariableReference] = full.allQueryRefs

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] = full.toTerm(queryName, debug)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Select
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class SelectQuery(full: FullSelectQuery.NonSubQuery) extends ParsedQuery {

    override val inputs: List[InputPart] = full.inputs
    override val ret: ReturningPart = full.ret
    override val refs: RefMap = full.refs

    override def show(using Quotes): String = full.show

    override protected[model] def allQueryRefs: Growable[VariableReference] = full.allQueryRefs

    def makeFragment(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      full.makeFragment

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] = full.toTerm(queryName, debug)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Update
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class UpdateQuery(full: FullUpdateQuery) extends ParsedQuery {

    override val inputs: List[InputPart] = full.inputs
    override val ret: ReturningPart = full.ret
    override val refs: RefMap = full.refs

    override def show(using Quotes): String = full.show

    override protected[model] def allQueryRefs: Growable[VariableReference] = full.allQueryRefs

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] = full.toTerm(queryName, debug)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Delete
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class DeleteQuery(full: FullDeleteQuery) extends ParsedQuery {

    override val inputs: List[InputPart] = full.inputs
    override val ret: ReturningPart = full.ret
    override val refs: RefMap = full.refs

    override def show(using Quotes): String = full.show

    override protected[model] def allQueryRefs: Growable[VariableReference] = full.allQueryRefs

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] = full.toTerm(queryName, debug)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Other
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[ParsedQuery] =
    PartialQuery.fullParserEmptyRefs
      .parse(input)
      .flatMap {
        case FullQueryResult(inputs, insert: PartialQuery.InsertQuery, ret, refs) =>
          FullInsertQuery.wrap(inputs, insert, ret, refs).map(ParsedQuery.InsertQuery(_))
        case FullQueryResult(inputs, select: PartialQuery.SelectQuery, ret, refs) =>
          FullSelectQuery.NonSubQuery.wrap(inputs, select, ret, refs).map(ParsedQuery.SelectQuery(_))
        case FullQueryResult(inputs, update: PartialQuery.UpdateQuery, ret, refs) =>
          FullUpdateQuery.wrap(inputs, update, ret, refs).map(ParsedQuery.UpdateQuery(_))
        case FullQueryResult(inputs, delete: PartialQuery.DeleteQuery, ret, refs) =>
          FullDeleteQuery.wrap(inputs, delete, ret, refs).map(ParsedQuery.DeleteQuery(_))
      }
      .map { parsed =>
        val specifiedQueryRefs: Set[VariableReference.InputLike] = parsed.refs.allQueryRefs.collect { case ref: VariableReference.InputLike => ref }.to[Set]
        val usedQueryRefs: Set[VariableReference.InputLike] = parsed.allQueryRefs.collect { case ref: VariableReference.InputLike => ref }.to[Set]
        val unusedQueryRefs: Set[VariableReference.InputLike] = specifiedQueryRefs &~ usedQueryRefs
        unusedQueryRefs.foreach { ref => report.warning("unused query input param", ref.param.tree.pos) }

        parsed
      }

  def compile(queryName: Expr[String], input: Term, debug: Boolean)(using Quotes): (ParsedQuery, Term) =
    ParseContext.root("compile", debug) {
      for {
        parsed <- ParseContext.add("parse") { ParsedQuery.parse(input) }
        res <- ParseContext.add(s"${parsed.productPrefix}.toTerm") { GenerationContext.root { parsed.toTerm(queryName, debug) } }
      } yield (parsed, res)
    }

}
