package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class InputPart(
    mapQueryRef: QueryParam.InputLike,
) {

  def show: String =
    s"""
         |  [INPUT] ${mapQueryRef.show}: ${mapQueryRef.param.tpe.showAnsiCode}""".stripMargin

}
object InputPart extends MapChainParser[InputPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[InputPart] = {
    for {
      (mapAAFC, input) <- AppliedAnonFunctCall.parseTyped[T.InputLike](term, "map function").parseLhsAndFunct {
        case ('{ Q.input.apply[a] }, mapAAFC)            => mapAAFC.funct.parseParam1.map { mapParam => InputPart(QueryParam.InputParam(mapParam)) }
        case ('{ Q.input.optional[a] }, mapAAFC)         => mapAAFC.funct.parseParam1.map { mapParam => InputPart(QueryParam.OptionalInputParam(mapParam)) }
        case ('{ Q.input.const[a](${ expr }) }, mapAAFC) => mapAAFC.funct.parseParam1.map { mapParam => InputPart(QueryParam.ConstInput(mapParam, expr.toTerm, TypeRepr.of[Any])) }
      }
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError

      newRefs = refs.add(input.mapQueryRef)

    } yield MapChainResult(input, mapFunctName, newRefs, mapAAFC.appliedFunctionBody)

  }

}
