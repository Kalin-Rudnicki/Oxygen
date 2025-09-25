package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class WherePart(
    filterExpr: QueryExpr,
) {

  def show(using Quotes): String =
    s"""
         |    WHERE ${filterExpr.show}""".stripMargin

}
object WherePart extends MapChainParser[WherePart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[WherePart] = {
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.Where](term, "map function").ignore
      _ <- mapAAFC.funct.parseEmptyParams
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      filterAAFC <- AppliedAnonFunctCall.parseTyped[T.Partial.Where](mapAAFC.lhs, "filter function").filterLhs { case '{ Q.where } => }.unknownAsError
      _ <- filterAAFC.funct.parseEmptyParams
      _ <- functionNames.withFilter.parse(filterAAFC.nameRef).unknownAsError

      filterExpr <- RawQueryExpr.parse((filterAAFC.appliedFunctionBody, refs)).unknownAsError
      filterExpr <- QueryExpr.parse(filterExpr).unknownAsError

    } yield MapChainResult(WherePart(filterExpr), mapFunctName, refs, mapAAFC.appliedFunctionBody)
  }

}
