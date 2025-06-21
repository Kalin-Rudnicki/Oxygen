package oxygen.sql.generic

import oxygen.quoted.*
import scala.quoted.*

private[generic] final case class FunctionCall(
    lhs: Term,
    nameRef: Ref,
    body: Function,
) {
  val name: String = nameRef.name
}
private[generic] object FunctionCall extends Parser[Term, FunctionCall] {

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[FunctionCall] = input match
    case singleApply(singleApply(ident: Ident, lhs), Function.required(function)) => function.map(FunctionCall(lhs, ident, _))
    case singleApply(select: Select, Function.required(function))                 => function.map(FunctionCall(select.qualifier, select, _))
    case _                                                                        => ParseResult.unknown(input, "not a function call")

}
