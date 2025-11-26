package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.T
import scala.quoted.*

final case class IntoPart(
    queryExpr: QueryExpr,
)
object IntoPart extends MapChainParser[IntoPart] {

  final case class FromSelect(into: IntoPart) {
    def toReturning: ReturningPart = ReturningPart(into.queryExpr.fullTerm, ReturningPart.Elem(into.queryExpr, None) :: Nil)
  }
  object FromSelect extends MapChainParser.Deferred[FromSelect] {

    override lazy val deferTo: MapChainParser[FromSelect] =
      IntoPart.map(FromSelect(_))

  }

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[IntoPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.InsertValues](term, "map function").ignore
      _ <- mapAAFC.funct.parseEmptyParams
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      (_, argTerm) <- mapAAFC.lhs match { // TODO (KR) : validate `intoIdent`
        case Apply(Select(intoIdent: Ident, "apply"), argTerm :: Nil) => ParseResult.Success((intoIdent, argTerm))
        case _                                                        => ParseResult.error(mapAAFC.lhs, "does not look like `into(...)`")
      }
      valueQueryExpr <- RawQueryExpr.parse((argTerm, refs)).unknownAsError
      valueQueryExpr <- QueryExpr.parse(valueQueryExpr).unknownAsError
    } yield MapChainResult(IntoPart(valueQueryExpr), mapFunctName, refs, mapAAFC.appliedFunctionBody)

}
