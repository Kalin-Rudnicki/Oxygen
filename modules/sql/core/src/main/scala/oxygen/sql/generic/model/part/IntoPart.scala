package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.T
import scala.quoted.*

final case class IntoPart(
    queryExpr: QueryExpr.UnaryInput.QueryRefIdent,
)
object IntoPart extends MapChainParser[IntoPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[IntoPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.InsertValues](term, "map function").ignore
      _ <- mapAAFC.funct.parseEmptyParams
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      (_, valueIdent) <- mapAAFC.lhs match { // TODO (KR) : validate `intoIdent`
        case Apply(Select(intoIdent: Ident, "apply"), (valueIdent: Ident) :: Nil) => ParseResult.Success((intoIdent, valueIdent))
        case _                                                                    => ParseResult.error(mapAAFC.lhs, "does not look like `into(...)`")
      }

      mapQueryRef <- refs.getInput(valueIdent)
      queryExpr = QueryExpr.UnaryInput.QueryRefIdent(valueIdent, mapQueryRef)
    } yield MapChainResult(IntoPart(queryExpr), mapFunctName, refs, mapAAFC.appliedFunctionBody)

}
