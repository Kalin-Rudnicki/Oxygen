package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.T
import oxygen.sql.schema.*
import scala.quoted.*

final case class JoinPart(
    mapQueryRef: QueryReference.Query,
    filterQueryRef: QueryReference.Query,
    filterExpr: QueryExpr,
    tableRepr: Expr[TableRepr[?]],
) {

  def show(using Quotes): String =
    s"""
         |    Join ${filterQueryRef.param.tpe.showAnsiCode} ${filterQueryRef.show}
         |      ON ${filterExpr.show}""".stripMargin

  def queryRefs: Growable[QueryReference] =
    mapQueryRef +: filterQueryRef +: filterExpr.queryRefs

}
object JoinPart extends MapChainParser[JoinPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[JoinPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.Join[?]](term, "map function").ignore
      mapParam <- mapAAFC.funct.parseParam1
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      (filterAAFC, tableRepr) <-
        AppliedAnonFunctCall
          .parseTyped[T.Partial.JoinLike](mapAAFC.lhs, "filter function")
          .parseLhs {
            // TODO (KR) : left join
            case '{ oxygen.sql.query.dsl.Q.join[a](using $tableRepr) } => ParseResult.Success(tableRepr)
            // case '{ oxygen.sql.query.dsl.Q.leftJoin[a](using $tableRepr) } => ParseResult.Success(tableRepr)
          }
          .unknownAsError
      filterParam <- filterAAFC.funct.parseParam1
      _ <- functionNames.withFilter.parse(filterAAFC.nameRef).unknownAsError

      mapQueryRef = QueryReference.Query(mapParam, tableRepr, false)
      filterQueryRef = QueryReference.Query(filterParam, tableRepr, false)
      newRefs = refs.add(
        mapQueryRef,
        filterQueryRef,
      )

      filterExpr <- RawQueryExpr.parse((filterAAFC.appliedFunctionBody, newRefs)).unknownAsError
      filterExpr <- QueryExpr.parse(filterExpr)

    } yield MapChainResult(JoinPart(mapQueryRef, filterQueryRef, filterExpr, tableRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

}
