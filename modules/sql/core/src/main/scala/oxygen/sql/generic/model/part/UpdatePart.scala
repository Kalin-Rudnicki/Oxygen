package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class UpdatePart(
    eitherMapQueryRef: Either[Function.RootParam.Ignored, QueryParam.Query],
    tableRepr: TypeclassExpr.TableRepr,
    mapSetParam: Function.NamedParam,
) {

  def queryRef: Option[QueryParam.Query] =
    eitherMapQueryRef.toOption

  def queryRefOrPlaceholder: QueryParam.Query =
    eitherMapQueryRef match {
      case Right(queryRef) => queryRef
      case Left(ignored)   => QueryParam.Query(Function.RootParam.Named(ignored.valDef), tableRepr, true)
    }

  def show: String =
    queryRef.fold("update(???)") { queryRef => s"${queryRef.param.tpe.showAnsiCode} ${queryRef.show}" }

}
object UpdatePart extends MapChainParser[UpdatePart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[UpdatePart] =
    for {
      (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Update[?]](term, "map function").parseLhs { //
        case '{ Q.update[a](using $tableRepr) } => ParseResult.Success(TypeclassExpr.TableRepr(tableRepr))
      }
      (mapParam, mapSetParam) <- mapAAFC.funct.parseParam2Either // (varRef, set) <- Q.update[_]
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

      mapQueryRef = mapParam.map { p1 => QueryParam.Query(p1, tableRepr, true) }
      update = UpdatePart(mapQueryRef, tableRepr, mapSetParam)
      newRefs = refs.add(update.queryRefOrPlaceholder)

    } yield MapChainResult(update, mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

}
