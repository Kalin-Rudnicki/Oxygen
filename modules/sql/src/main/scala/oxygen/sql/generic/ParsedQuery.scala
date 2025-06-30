package oxygen.sql.generic

import oxygen.quoted.*
import scala.quoted.*

private[sql] sealed trait ParsedQuery extends Product {

  val inputs: List[QueryParser.Input]
  val ret: QueryParser.Returning

  def show(using Quotes): String

  protected final def showInputs: String =
    if (inputs.nonEmpty) inputs.map(_.show).mkString("inputs:", "", "\n")
    else "<no inputs>\n"

  protected final def showReturning(using Quotes): String =
    ret.showOpt.map { ret => s"\n    RETURNING $ret" }.mkString

  def toTerm(using ParseContext, Quotes): ParseResult[Term]

}
private[sql] object ParsedQuery extends Parser[Term, ParsedQuery] {

  final case class InsertQuery(
      inputs: List[QueryParser.Input],
      insert: QueryParser.InsertQ,
      into: QueryParser.Into,
      ret: QueryParser.Returning,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |INSERT INTO ${insert.show}
         |    VALUES ${into.queryRef.show}$showReturning
         |""".stripMargin

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      ParseResult.error(Position.ofMacroExpansion, "TODO")

  }

  final case class SelectQuery(
      inputs: List[QueryParser.Input],
      select: QueryParser.SelectQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      ret: QueryParser.Returning,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |SELECT ${ret.showOpt.getOrElse("<ERROR>")}
         |    ${select.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |""".stripMargin

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      ParseResult.error(Position.ofMacroExpansion, "TODO")

  }

  final case class UpdateQuery(
      inputs: List[QueryParser.Input],
      update: QueryParser.UpdateQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      set: QueryParser.Set,
      ret: QueryParser.Returning,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |UPDATE ${update.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |${set.show}$showReturning
         |""".stripMargin

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      ParseResult.error(Position.ofMacroExpansion, "TODO")

  }

  final case class DeleteQuery(
      inputs: List[QueryParser.Input],
      delete: QueryParser.DeleteQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      ret: QueryParser.Returning,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |DELETE FROM ${delete.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}$showReturning
         |""".stripMargin

    override def toTerm(using ParseContext, Quotes): ParseResult[Term] =
      ParseResult.error(Position.ofMacroExpansion, "TODO")

  }

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[ParsedQuery] =
    QueryParser.finished.parse(input).map {
      case (inputs, QueryParser.PartialQuery.InsertQuery(insert, into), ret) =>
        ParsedQuery.InsertQuery(inputs, insert, into, ret)
      case (inputs, QueryParser.PartialQuery.SelectQuery(select, joins, where), ret) =>
        ParsedQuery.SelectQuery(inputs, select, joins, where, ret)
      case (inputs, QueryParser.PartialQuery.UpdateQuery(update, joins, where, set), ret) =>
        ParsedQuery.UpdateQuery(inputs, update, joins, where, set, ret)
      case (inputs, QueryParser.PartialQuery.DeleteQuery(delete, joins, where), ret) =>
        ParsedQuery.DeleteQuery(inputs, delete, joins, where, ret)
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
