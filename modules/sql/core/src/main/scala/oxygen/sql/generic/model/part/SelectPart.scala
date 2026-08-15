package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

sealed trait SelectPart {

  def show(using Quotes): String

  final def optTableRepr: Option[TypeclassExpr.TableRepr] = this match
    case SelectPart.FromTable(_, tableRepr) => tableRepr.some
    case _: SelectPart.FromSubQuery         => None
    case _: SelectPart.FromUnnest           => None

}
object SelectPart extends MapChainParser.Deferred[SelectPart] {

  final case class FromTable(
      mapQueryRef: VariableReference.FromQuery,
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

        mapQueryRef = VariableReference.FromQuery(mapParam, tableRepr, true)
        newRefs = refs.add(mapQueryRef)

      } yield MapChainResult(FromTable(mapQueryRef, tableRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

  final case class FromSubQuery(
      subQueryTableName: String,
      mapQueryRefs: NonEmptyList[VariableReference.FromQuery],
      subQuery: ParsedQuery.SelectQuery,
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
        select <- ParseContext.add("SubQuery") { ParsedQuery.SelectQuery.parse(subQueryTerm).unknownAsError }

        _ <- ParseResult.validate(mapParams.size == select.ret.returningExprs.size)(select.ret.fullTree, "Number of subQuery params and sub query returning do not match?")
        zippedParamPairs <- mapParams.zip(NonEmptyList.unsafeFromList(select.ret.returningExprs)).traverse { case (param, ret) =>
          ret.expr.getRowRepr.map { rowRepr =>
            val retAs = param.name.camelToSnake
            val subQueryRef = s"($subQueryTableName.$retAs)"
            (
              VariableReference.FromQuery.subquery(param, rowRepr, false, subQueryRef),
              ReturningPart.Elem(ret.expr, retAs.some),
            )
          }
        }
        zippedParams = zippedParamPairs.map(_._1)
        modifiedSelect = select.copy(
          ret = select.ret.copy(returningExprs = zippedParamPairs.toList.map(_._2)),
        )

        newRefs = refs.addList(zippedParams.toList)

      } yield MapChainResult(FromSubQuery(subQueryTableName, zippedParams, modifiedSelect), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromUnnest
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * `FROM UNNEST(?::type[]) alias` : uses an array/collection input (`arrInput`) as a table source
    * that yields one row per element. The element is exposed as the query var `mapQueryRef`
    * (a single, alias-named column) so it can be joined/filtered/selected against.
    */
  final case class FromUnnest(
      mapQueryRef: VariableReference.FromQuery,
      arrInput: QueryExpr.InputVariableReferenceLike,
      elemRowRepr: TypeclassExpr.RowRepr,
  ) extends SelectPart {

    override def show(using Quotes): String =
      s"FROM UNNEST(${arrInput.show}) ${mapQueryRef.show}"

    def queryRefs: Growable[VariableReference] =
      mapQueryRef +: arrInput.queryRefs

  }
  object FromUnnest extends MapChainParser[FromUnnest] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[FromUnnest] =
      for {
        (mapAAFC, (elemRowRepr, arrTerm)) <- AppliedAnonFunctCall.parseTyped[T.SelectUnnest[?]](term, "map function").parseLhs { //
          case '{ Q.select.unnest[a]($arr)(using $rowRepr) } => ParseResult.success((TypeclassExpr.RowRepr(rowRepr), arr.toTerm.underlyingArgument))
        }
        mapParam <- mapAAFC.funct.parseParam1
        mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

        // resolve the array input that is being unnested (must be a previously-declared `input.array[_]`)
        rawArr <- RawQueryExpr.VariableReferenceLike.parse((arrTerm, refs)).unknownAsError
        arrInput <- QueryExpr.InputVariableReferenceLike.parse(rawArr)

        // the element becomes a query var; name its single column = the alias, so it is referenced as
        // `alias.alias` (qualified), avoiding ambiguity with same-named columns of joined tables.
        alias = mapParam.name.camelToSnake
        namedElemRowRepr = elemRowRepr.prefixedInline(alias)
        mapQueryRef = VariableReference.FromQuery(mapParam, namedElemRowRepr, true)
        newRefs = refs.add(mapQueryRef)

      } yield MapChainResult(FromUnnest(mapQueryRef, arrInput, namedElemRowRepr), mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

  override lazy val deferTo: MapChainParser[SelectPart] = FromTable || FromSubQuery || FromUnnest

}
