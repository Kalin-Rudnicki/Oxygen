package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

final case class ReturningPart(
    fullTree: Tree,
    returningExprs: List[ReturningPart.Elem],
) {

  def showOpt(using Quotes): Option[String] = returningExprs match
    case Nil         => None
    case expr :: Nil => expr.expr.show.some
    case exprs       => exprs.map(_.expr.show).mkString(", ").some

}
object ReturningPart extends Parser[(Term, RefMap), ReturningPart] {

  final case class Elem(
      expr: QueryExpr,
      as: Option[String],
  )

  override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[ReturningPart] =
    for {
      (term, refs) <- ParseResult.Success(input)
      terms = term match
        case unitExpr()       => Nil
        case tupleApply(args) => args.toList
        case _                => term :: Nil
      exprs <- terms.traverse(t => RawQueryExpr.parse((t, refs)))
      exprs <- exprs.traverse(t => QueryExpr.parse(t))
    } yield ReturningPart(term, exprs.map(Elem(_, None)))

}
