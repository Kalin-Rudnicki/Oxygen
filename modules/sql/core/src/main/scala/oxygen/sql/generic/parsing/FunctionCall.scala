package oxygen.sql.generic.parsing

import oxygen.quoted.*
import scala.quoted.*

/**
  * ------------------------- ... -------------------------
  * ((fc2.lhs))
  *     .((fc2.nameRef)) { ((fc2.funct.params)) => ((fc2.funct.body)) }
  *     .((fc1.nameRef)) { ((fc1.funct.params)) => ((fc1.funct.body)) }
  * ------------------------- ... -------------------------
  * ((fc1.lhs))
  *     .((fc1.nameRef)) { ((fc1.funct.params)) => ((fc1.funct.body)) }
  */
private[generic] final case class FunctionCall(
    lhs: Term,
    nameRef: Ref,
    funct: Function,
) {
  val name: String = nameRef.name
}
private[generic] object FunctionCall extends Parser[Term, FunctionCall] {

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[FunctionCall] = input match
    case singleApply(singleApply(ident: Ident, lhs), Function.required(function)) => function.map(FunctionCall(lhs, ident, _))
    case singleApply(select: Select, Function.required(function))                 => function.map(FunctionCall(select.qualifier, select, _))
    case _                                                                        => ParseResult.unknown(input, "not a function call")

  final class ParseTypedBuilder[LhsTpe: Type](term: Term, ctx: String) {

    def parseLhsAndFunct[A](f: PartialFunction[(Expr[LhsTpe], FunctionCall), ParseResult[A]])(using ParseContext, Quotes): ParseResult[(FunctionCall, A)] =
      ParseContext.add(ctx) {
        for {
          fun <- FunctionCall.parse(term)
          lhsTpe = fun.lhs.tpe
          expLhsTpeRepr = TypeRepr.of[LhsTpe]
          lhsExpr <-
            if (lhsTpe <:< expLhsTpeRepr) ParseResult.Success(fun.lhs.asExprOf[LhsTpe])
            else ParseResult.unknown(fun.lhs, s"function LHS `${lhsTpe.showAnsiCode}` is not a subtype of `${expLhsTpeRepr.showAnsiCode}`")
          a <- f.applyOrElse((lhsExpr, fun), _ => ParseResult.error(fun.lhs, "invalid function LHS"))
        } yield (fun, a)
      }

    def parseLhs[A](f: PartialFunction[Expr[LhsTpe], ParseResult[A]])(using ParseContext, Quotes): ParseResult[(FunctionCall, A)] =
      parseLhsAndFunct[A] { case (f(res), _) => res }

    def filterLhs(f: PartialFunction[Expr[LhsTpe], Unit])(using ParseContext, Quotes): ParseResult[FunctionCall] =
      parseLhs[Unit] { case f(res) => ParseResult.Success(res) }.map(_._1)

    def ignore(using ParseContext, Quotes): ParseResult[FunctionCall] =
      parseLhsAndFunct[Unit] { _ => ParseResult.Success(()) }.map(_._1)

  }

  def parseTyped[LhsTpe: Type](term: Term, ctx: String): ParseTypedBuilder[LhsTpe] = new ParseTypedBuilder[LhsTpe](term, ctx)

}
