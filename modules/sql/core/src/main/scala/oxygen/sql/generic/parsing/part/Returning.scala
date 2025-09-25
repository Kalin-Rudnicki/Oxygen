package oxygen.sql.generic.parsing.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

final case class Returning(tree: Tree, exprs: List[QueryExpr]) {

  def showOpt(using Quotes): Option[String] = exprs match
    case Nil         => None
    case expr :: Nil => expr.show.some
    case exprs       => exprs.map(_.show).mkString(", ").some

}
object Returning extends Parser[(Term, RefMap), Returning] {

  override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[Returning] =
    for {
      (term, refs) <- ParseResult.Success(input)
      terms = term match
        case unitExpr()       => Nil
        case tupleApply(args) => args.toList
        case _                => term :: Nil
      exprs <- terms.traverse(t => RawQueryExpr.parse((t, refs)))
      exprs <- exprs.traverse(t => QueryExpr.parse(t))
    } yield Returning(term, exprs)

}
