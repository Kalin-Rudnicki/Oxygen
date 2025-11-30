package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.full.FullSelectQuery
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

sealed trait SelectPart {

  def show(using Quotes): String

  final def optTableRepr: Option[TypeclassExpr.TableRepr] = this match
    case SelectPart.FromTable(_, tableRepr) => tableRepr.some
    case _: SelectPart.FromSubQuery         => None

}
object SelectPart extends MapChainParser.Deferred[SelectPart] {

  final case class FromTable(
      mapQueryRef: VariableReference.QueryTableReference,
      tableRepr: TypeclassExpr.TableRepr,
  ) extends SelectPart {

    override def show(using Quotes): String =
      s"FROM ${mapQueryRef.tpe.showAnsiCode} ${mapQueryRef.show}"

  }
  object FromTable extends MapChainParser[FromTable] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[FromTable] =
      for {
        (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Select[?]](term, "map function").parseLhs { //
          case '{ Q.select[a](using $tableRepr) } => ParseResult.Success(TypeclassExpr.TableRepr(tableRepr))
        }
        mapParam <- mapAAFC.funct.parseParam1
        mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

        mapQueryRef = VariableReference.QueryTableReference(mapParam, tableRepr, true)
        newRefs = refs.add(mapQueryRef)

      } yield MapChainResult(FromTable(mapQueryRef, tableRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

  final case class FromSubQuery(
      subQueryTableName: String,
      mapQueryRefs: NonEmptyList[VariableReference.SubQueryReference],
      subQuery: FullSelectQuery.SubQuery,
  ) extends SelectPart {

    override def show(using Quotes): String =
      s"FROM (\n  ${subQuery.show.replaceAll("\n", "\n    ")}\n) AS $subQueryTableName"

  }
  object FromSubQuery extends MapChainParser[FromSubQuery] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[FromSubQuery] =
      for {
        (mapAAFC, (subQueryTableName, subQueryTerm)) <- AppliedAnonFunctCall.parseTyped[T.SelectSubQuery[?]](term, "map function").parseLhs { //
          case '{ Q.select.subQuery[a](${ Expr(subQueryTableName) })(${ subQueryExpr }) } => ParseResult.success((subQueryTableName, subQueryExpr.toTerm))
        }
        mapParams <- mapAAFC.funct.parseNonEmptyParams
        mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
        select <- ParseContext.add("SubQuery") { FullSelectQuery.Basic.parse((subQueryTerm, refs)).unknownAsError }

        basicRet: ReturningPart.BasicNel = select.ret

        _ <- ParseResult.validate(mapParams.size == basicRet.returningExprsNel.size)(select.ret.fullTree, "Number of subQuery params and sub query returning do not match?")
        zippedParamPairs <- mapParams.zip(basicRet.returningExprsNel).traverse { case (param, retElem) =>
          retElem.expr match {
            case retExpr: QueryExpr.QueryVariableReferenceLike.ReferencedVariable =>
              val retAs = param.name.camelToSnake
              val res =
                (
                  VariableReference.SubQueryReference(param, retExpr.queryRef.tableRepr, retExpr.queryRef.rowRepr, false, subQueryTableName, retAs),
                  ReturningPart.Elem.SubQuery(retExpr, retAs): ReturningPart.Elem.SubQuery,
                )
              ParseResult.success(res)
            case invalid =>
              ParseResult.error(invalid.fullTerm, "for now, sub-queries are only allowed to return entire rows")
          }
        }

        zippedParams = zippedParamPairs.map(_._1)
        modifiedSelect = FullSelectQuery.SubQuery(
          inputs = select.inputs,
          select = select.select,
          joins = select.joins,
          where = select.where,
          orderBy = select.orderBy,
          limit = select.limit,
          offset = select.offset,
          ret = ReturningPart.SubQuery(basicRet.fullTree, zippedParamPairs.map(_._2)),
          refs = select.refs,
        )

        newRefs = refs.addList(zippedParams.toList)

      } yield MapChainResult(FromSubQuery(subQueryTableName, zippedParams, modifiedSelect), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

  override lazy val deferTo: MapChainParser[SelectPart] = FromTable || FromSubQuery

}
