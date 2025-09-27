package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

sealed trait SelectPart {

  val mapQueryRef: QueryParam.Query

  def show(using Quotes): String

}
object SelectPart extends MapChainParser[SelectPart] {

  final case class FromTable(
      mapQueryRef: QueryParam.Query,
      tableRepr: TypeclassExpr.TableRepr,
  ) extends SelectPart {

    override def show(using Quotes): String =
      s"FROM ${mapQueryRef.param.tpe.showAnsiCode} ${mapQueryRef.show}"

  }
  object FromTable extends MapChainParser[FromTable] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[FromTable] =
      for {
        (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Select[?]](term, "map function").parseLhs { //
          case '{ Q.select[a](using $tableRepr) } => ParseResult.Success(TypeclassExpr.TableRepr(tableRepr))
        }
        mapParam <- mapAAFC.funct.parseParam1
        mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

        mapQueryRef = QueryParam.Query(mapParam, tableRepr, true)
        newRefs = refs.add(mapQueryRef)

      } yield MapChainResult(FromTable(mapQueryRef, tableRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

  final case class FromSubQuery(
      mapQueryRef: QueryParam.Query,
      subquery: ParsedQuery.SelectQuery,
  ) extends SelectPart {

    override def show(using Quotes): String =
      s"FROM ${subquery.show}"

  }
  object FromSubQuery extends MapChainParser[FromSubQuery] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[FromSubQuery] =
      for {
        (mapAAFC, subQueryExpr) <- AppliedAnonFunctCall.parseTyped[T.SelectSubQuery[?]](term, "map function").parseLhs { //
          case '{ Q.select.subquery[a] { $subQueryExpr } } => ParseResult.Success($subQueryExpr)
        }
        mapParam <- mapAAFC.funct.parseParam1
        mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

        subquery <- ParseContext.add("subquery") { ParsedQuery.SelectQuery.parse(subQueryExpr.toTerm) }
        mapQueryRef = QueryParam.Query(mapParam, tableRepr, true)
        newRefs = refs.add(mapQueryRef)

      } yield MapChainResult(FromSubQuery(mapQueryRef, subquery), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

  val orParser: MapChainParser[SelectPart] = FromTable || FromSubQuery

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[SelectPart] = orParser.parse(term, refs, prevFunction)

}
