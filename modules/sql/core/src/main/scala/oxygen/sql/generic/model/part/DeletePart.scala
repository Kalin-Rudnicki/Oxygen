package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class DeletePart(
    mapQueryRef: VariableReference.FromQuery,
    tableRepr: TypeclassExpr.TableRepr,
) {

  def show: String =
    s"${mapQueryRef.tpe.showAnsiCode} ${mapQueryRef.show}"

}
object DeletePart extends MapChainParser[DeletePart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[DeletePart] =
    for {
      (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Delete[?]](term, "map function").parseLhs { //
        case '{ Q.delete[a](using $tableRepr) } => ParseResult.Success(TypeclassExpr.TableRepr(tableRepr))
      }
      mapParam <- mapAAFC.funct.parseParam1
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

      mapQueryRef = VariableReference.FromQuery(mapParam, tableRepr, true)
      newRefs = refs.add(mapQueryRef)

    } yield MapChainResult(DeletePart(mapQueryRef, tableRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

}
