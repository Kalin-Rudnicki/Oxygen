package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class InsertPart(
    mapQueryRef: Option[QueryParam.Query],
    tableRepr: TypeclassExpr.TableRepr,
    mapIntoParam: Function.NamedParam,
) {

  def rowRepr: TypeclassExpr.RowRepr = tableRepr.tableRowRepr

  def show: String =
    mapQueryRef.fold("insert(???)")(queryRef => s"${queryRef.param.tpe.showAnsiCode} ${queryRef.show}")

}
object InsertPart extends MapChainParser[InsertPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[InsertPart] =
    for {
      (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Insert[?]](term, "map function").parseLhs { //
        case '{ Q.insert[a](using $tableRepr) } => ParseResult.Success(TypeclassExpr.TableRepr(tableRepr))
      }
      (mapParam, mapIntoParam) <- mapAAFC.funct.parseParam2Opt // (varRef, into) <- Q.insert[_]
      f1Name <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

      mapQueryRef = mapParam.map { p1 => QueryParam.Query(p1, tableRepr, true) }
      newRefs = refs.add(mapQueryRef.toList*)

    } yield MapChainResult(InsertPart(mapQueryRef, tableRepr, mapIntoParam), f1Name, newRefs, mapAAFC.appliedFunctionBody)

}
