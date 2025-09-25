package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import oxygen.sql.schema.*
import scala.quoted.*

final case class InsertPart(
    mapQueryRef: Option[QueryReference.Query],
    tableRepr: Expr[TableRepr[?]],
    mapIntoParam: Function.Param,
) {

  def rowRepr(using Quotes): Expr[RowRepr[?]] =
    '{ $tableRepr.rowRepr }

  def show: String =
    mapQueryRef.fold("insert(???)")(queryRef => s"${queryRef.param.tpe.showAnsiCode} ${queryRef.show}")

}
object InsertPart extends MapChainParser[InsertPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[InsertPart] =
    for {
      (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Insert[?]](term, "map function").parseLhs { case '{ Q.insert[a](using $tableRepr) } => ParseResult.Success(tableRepr) }
      (mapParam, mapIntoParam) <- mapAAFC.funct.parseParam2Opt // (varRef, into) <- Q.insert[_]
      f1Name <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

      mapQueryRef = mapParam.map { p1 => QueryReference.Query(p1, tableRepr, true) }
      newRefs = refs.add(mapQueryRef.toList*)

    } yield MapChainResult(InsertPart(mapQueryRef, tableRepr, mapIntoParam), f1Name, newRefs, mapAAFC.appliedFunctionBody)

}
