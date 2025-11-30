package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.T
import scala.quoted.*

final case class JoinPart(
    joinType: JoinPart.JoinType,
    mapQueryRef: VariableReference.QueryLike,
    filterQueryRef: VariableReference.QueryLike,
    filterExpr: QueryExpr,
    tableRepr: TypeclassExpr.TableRepr,
) {

  def show(using Quotes): String = {
    val joinStr = joinType match
      case JoinPart.JoinType.Inner     => "Join"
      case JoinPart.JoinType.LeftOuter => "Left Join"

    s"""
       |    $joinStr ${filterQueryRef.tpe.showAnsiCode} ${filterQueryRef.show}
       |      ON ${filterExpr.show}""".stripMargin
  }

  def queryRefs: Growable[VariableReference] =
    mapQueryRef +: filterQueryRef +: filterExpr.queryRefs

}
object JoinPart extends MapChainParser[JoinPart] {

  enum JoinType { case Inner, LeftOuter }

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[JoinPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.Join[?]](term, "map function").ignore
      mapParam <- mapAAFC.funct.parseParam1
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      (filterAAFC, (tableRepr, joinType)) <-
        AppliedAnonFunctCall
          .parseTyped[T.Partial.JoinLike](mapAAFC.lhs, "filter function")
          .parseLhs {
            case '{ oxygen.sql.query.dsl.Q.join[a](using $tableRepr) }     => ParseResult.Success((TypeclassExpr.TableRepr(tableRepr), JoinType.Inner))
            case '{ oxygen.sql.query.dsl.Q.leftJoin[a](using $tableRepr) } => ParseResult.Success((TypeclassExpr.TableRepr(tableRepr), JoinType.LeftOuter))
          }
          .unknownAsError
      filterParam <- filterAAFC.funct.parseParam1
      _ <- functionNames.withFilter.parse(filterAAFC.nameRef).unknownAsError

      mapQueryRef = joinType match
        case JoinType.Inner     => VariableReference.QueryTableReference(mapParam, tableRepr, false)
        case JoinType.LeftOuter => VariableReference.QueryTableReference(mapParam, tableRepr, tableRepr.tableRowRepr.optional, false)
      filterQueryRef = VariableReference.QueryTableReference(filterParam, tableRepr, false)
      newRefs = refs.add(
        mapQueryRef,
        filterQueryRef,
      )

      filterExpr <- RawQueryExpr.parse((filterAAFC.appliedFunctionBody, newRefs)).unknownAsError
      filterExpr <- QueryExpr.parse(filterExpr)

    } yield MapChainResult(JoinPart(joinType, mapQueryRef, filterQueryRef, filterExpr, tableRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

}
