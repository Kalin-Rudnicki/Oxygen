package oxygen.sql.generic.parsing.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.T
import scala.quoted.*

final case class Into(
    queryExpr: QueryExpr.InputLike.QueryRefIdent,
)
object Into extends QueryParser[Into] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Into, String, RefMap, Term)] =
    for {
      fc1 <- FunctionCall.parseTyped[T.InsertValues](term, "function 1").ignore
      _ <- fc1.funct.parseEmptyParams
      f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
      (_, valueIdent) <- fc1.lhs match { // TODO (KR) : validate `intoIdent`
        case Apply(Select(intoIdent: Ident, "apply"), (valueIdent: Ident) :: Nil) => ParseResult.Success((intoIdent, valueIdent))
        case _                                                                    => ParseResult.error(fc1.lhs, "does not look like `into(...)`")
      }

      queryRef <- refs.getInput(valueIdent)
      queryExpr = QueryExpr.InputLike.QueryRefIdent(valueIdent, queryRef)
    } yield (Into(queryExpr), f1Name, refs, fc1.funct.body)

}
