package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

sealed trait InsertPart {
  val mapQueryRef: Option[VariableReference.FromQuery]
  val tableRepr: TypeclassExpr.TableRepr
  val mapIntoParam: Function.NamedParam

  final def rowRepr: TypeclassExpr.RowRepr = tableRepr.tableRowRepr

  final def show: String =
    mapQueryRef.fold("insert(???)")(queryRef => s"${queryRef.tpe.showAnsiCode} ${queryRef.show}")

}
object InsertPart {

  final case class Basic(
      mapQueryRef: Option[VariableReference.FromQuery],
      tableRepr: TypeclassExpr.TableRepr,
      mapIntoParam: Function.NamedParam,
  ) extends InsertPart
  object Basic extends MapChainParser[Basic] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[Basic] =
      for {
        (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.Insert[?]](term, "map function").parseLhs { //
          case '{ Q.insert[a](using $tableRepr) } => ParseResult.Success(TypeclassExpr.TableRepr(tableRepr))
        }
        (mapParam, mapIntoParam) <- mapAAFC.funct.parseParam2Opt // (varRef, into) <- Q.insert[_]
        f1Name <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

        mapQueryRef = mapParam.map { p1 => VariableReference.FromQuery(p1, tableRepr, true) }
        newRefs = refs.add(mapQueryRef.toList*)

      } yield MapChainResult(Basic(mapQueryRef, tableRepr, mapIntoParam), f1Name, newRefs, mapAAFC.appliedFunctionBody)

  }

  final case class FromSelect(
      mapQueryRef: Option[VariableReference.FromQuery],
      tableRepr: TypeclassExpr.TableRepr,
      mapIntoParam: Function.NamedParam,
  ) extends InsertPart
  object FromSelect extends MapChainParser[FromSelect] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[FromSelect] =
      for {
        (mapAAFC, tableRepr) <- AppliedAnonFunctCall.parseTyped[T.InsertFromSelect[?]](term, "map function").parseLhs { //
          case '{ Q.insert.fromSelect[a](using $tableRepr) } => ParseResult.Success(TypeclassExpr.TableRepr(tableRepr))
        }
        (mapParam, mapIntoParam) <- mapAAFC.funct.parseParam2Opt // (varRef, into) <- Q.insert[_]
        f1Name <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

        mapQueryRef = mapParam.map { p1 => VariableReference.FromQuery(p1, tableRepr, true) }
        newRefs = refs.add(mapQueryRef.toList*)

      } yield MapChainResult(FromSelect(mapQueryRef, tableRepr, mapIntoParam), f1Name, newRefs, mapAAFC.appliedFunctionBody)

  }

}
