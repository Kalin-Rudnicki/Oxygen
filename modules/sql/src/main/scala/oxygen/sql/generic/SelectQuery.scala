package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.quoted.*
import scala.quoted.*

private[generic] final case class SelectQuery(
    from: QueryBlock.From,
    joins: List[QueryBlock.Join],
    where: Option[QueryBlock.Where],
    returning: QueryBlock.Returning,
) {

  def show(rootParams: List[Function.Param]): String = {
    val paramStrs: List[String] =
      if (rootParams.nonEmpty) "INPUTS:" +: (rootParams.map { p => s"  ${p.name.cyanFg}: ${p.tpe.showCode}," } :+ "")
      else Nil

    val allStrs: List[List[String]] =
      List(
        paramStrs,
        List(returning.show, from.show),
        joins.map(_.show),
        where.map(_.show).toList,
      )

    allStrs.flatten.mkString("\n")
  }

}
private[generic] object SelectQuery extends Parser[(Term, RefMap), SelectQuery] {

  private val parts: Parser[(Term, RefMap), (QueryBlock.From, List[QueryBlock.Join], Option[QueryBlock.Where], QueryBlock.Returning)] =
    QueryBlock.From.withContext("From") >>>
      QueryBlock.Join.many.withContext("Join") >>>
      QueryBlock.Where.maybe.withContext("Where") >>>
      QueryBlock.Returning

  override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[SelectQuery] =
    parts.parse(input).map(SelectQuery.apply)

}
