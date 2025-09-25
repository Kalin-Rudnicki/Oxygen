package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import oxygen.sql.schema.*
import scala.quoted.*

final case class SelectPart(
    mapQueryRef: QueryReference.Query,
    tableRepr: Expr[TableRepr[?]],
) {

  def show: String =
    s"FROM ${mapQueryRef.param.tpe.showAnsiCode} ${mapQueryRef.show}"

}
object SelectPart extends MapChainParser[SelectPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[SelectPart] =
    for {
      (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Select[?]](term, "map function").parseLhs { case '{ Q.select[a](using $tableRepr) } => ParseResult.Success(tableRepr) }
      mapParam <- mapAAFC.funct.parseParam1
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

      mapQueryRef = QueryReference.Query(mapParam, tableRepr, true)
      newRefs = refs.add(mapQueryRef)

    } yield MapChainResult(SelectPart(mapQueryRef, tableRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

}
