package oxygen.sql.generic.parsing.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class Input(
    queryRef: QueryReference.InputLike,
) {

  def show: String =
    s"""
         |  [INPUT] ${queryRef.show}: ${queryRef.param.tpe.showAnsiCode}""".stripMargin

}
object Input extends QueryParser[Input] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Input, String, RefMap, Term)] = {
    for {
      (fc1, input) <- FunctionCall.parseTyped[T.InputLike](term, "function 1").parseLhsAndFunct {
        case ('{ Q.input.apply[a] }, fc1)            => fc1.funct.parseParam1.map { p1 => Input(QueryReference.InputParam(p1)) }
        case ('{ Q.input.const[a](${ expr }) }, fc1) => fc1.funct.parseParam1.map { p1 => Input(QueryReference.ConstInput(p1, expr.toTerm, TypeRepr.of[Any])) }
      }
      f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError

      newRefs = refs.add(input.queryRef)

    } yield (input, f1Name, newRefs, fc1.funct.body)

  }

}
