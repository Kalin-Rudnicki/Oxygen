package oxygen.sql.generic

import oxygen.quoted.*
import scala.quoted.*

private[sql] sealed trait ParsedQuery {

  def toTerm(using Quotes): Term =
    report.errorAndAbort("TODO : ParsedQuery.toTerm")

}
private[sql] object ParsedQuery extends Parser[Term, ParsedQuery] {

  final case class InsertQuery(
      inputs: List[QueryParser.Input],
      insert: QueryParser.Insert,
      into: QueryParser.Into,
      ret: QueryParser.Returning,
  ) extends ParsedQuery

  final case class SelectQuery(
      inputs: List[QueryParser.Input],
      select: QueryParser.Select,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      ret: QueryParser.Returning,
  ) extends ParsedQuery

  final case class UpdateQuery(
      inputs: List[QueryParser.Input],
      update: QueryParser.Update,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      set: QueryParser.Set,
      ret: QueryParser.Returning,
  ) extends ParsedQuery

  final case class DeleteQuery(
      inputs: List[QueryParser.Input],
      delete: QueryParser.Delete,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      ret: QueryParser.Returning,
  ) extends ParsedQuery

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

}
