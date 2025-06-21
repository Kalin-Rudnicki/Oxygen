package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] object singleApply {

  def unapply(term: Term): Option[(Term, Term)] = term match
    case Apply(TypeApply(fun, _), arg :: Nil) => (fun, arg).some
    case Apply(fun, arg :: Nil)               => (fun, arg).some
    case _                                    => None

}

private[generic] object manyApply {

  def unapply(term: Term): Option[(Term, List[Term])] = term match
    case Apply(TypeApply(fun, _), args) => (fun, args).some
    case Apply(fun, args)               => (fun, args).some
    case _                              => None

}

private[generic] object tupleApply {

  def unapply(term: Term): Option[NonEmptyList[Term]] = term match
    case manyApply(Select(Ident(ident), "apply"), args) if ident.startsWith("Tuple") => args.toNonEmpty
    case _                                                                           => None

}

private[generic] object functionCall extends Parser[Term, (Term, Ident | Select, Function)] {

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[(Term, Ident | Select, Function)] = input match
    case singleApply(singleApply(ident: Ident, lhs), Function.required(function)) => function.map((lhs, ident, _))
    case singleApply(select: Select, Function.required(function))                 => function.map((select.qualifier, select, _))
    case _                                                                        => ParseResult.unknown(input, "not a function call")

}

private[generic] object functionNames {

  extension (self: Ident | Select)
    private def getName: String = self match
      case Ident(name)     => name
      case Select(_, name) => name

  object mapOrFlatMap extends Parser[Ident | Select, String] {

    override def parse(input: Ident | Select)(using ParseContext, Quotes): ParseResult[String] = input.getName match
      case name @ ("flatMap" | "map") => ParseResult.Success(name)
      case name                       => ParseResult.unknown(input, s"expected `map`/`flatMap`, got: $name")

  }

  object withFilter extends Parser[Ident | Select, String] {

    override def parse(input: Ident | Select)(using ParseContext, Quotes): ParseResult[String] = input.getName match
      case name @ "withFilter" => ParseResult.Success(name)
      case name                => ParseResult.unknown(input, s"expected `withFilter`, got: $name")

  }

}
