package oxygen.sql.generic.parsing

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] object singleApply {

  extension (flags: Flags)
    private def isImplicit(using Quotes): Boolean =
      flags.is(Flags.Given) || flags.is(Flags.Implicit)

  extension (term: Term)
    private def removeImplicitApplies(using Quotes): Term = term match {
      case Apply(fun, args) if args.nonEmpty && args.forall(_.symbol.flags.isImplicit) => fun.removeImplicitApplies
      case _                                                                           => term
    }

  def unapply(term: Term)(using Quotes): Option[(Term, Term)] = term.removeImplicitApplies match
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

private[generic] object unitExpr {

  def unapply(term: Term)(using Quotes): Boolean = term.asExpr match
    case '{ () } => true
    case _       => false

}

private[generic] object tupleApply {

  def unapply(term: Term): Option[NonEmptyList[Term]] = term match
    case manyApply(Select(Ident(ident), "apply"), args) if ident.startsWith("Tuple") => args.toNonEmpty
    case _                                                                           => None

}

private[generic] object functionNames {

  object mapOrFlatMap extends Parser[Ref, String] {

    override def parse(input: Ref)(using ParseContext, Quotes): ParseResult[String] = input.name match
      case name @ ("flatMap" | "map") => ParseResult.Success(name)
      case name                       => ParseResult.unknown(input, s"expected `map`/`flatMap`, got: $name")

  }

  object withFilter extends Parser[Ref, String] {

    override def parse(input: Ref)(using ParseContext, Quotes): ParseResult[String] = input.name match
      case name @ "withFilter" => ParseResult.Success(name)
      case name                => ParseResult.unknown(input, s"expected `withFilter`, got: $name")

  }

}
