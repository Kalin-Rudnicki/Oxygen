package oxygen.quoted.companion

import oxygen.quoted.*
import scala.quoted.*

final class ImplicitsCompanion(using quotes: Quotes) {

  /**
    * Find a given instance of type `T` in the current scope provided by the current enclosing splice.
    *  Return an `ImplicitSearchResult`.
    *
    *  @param tpe type of the implicit parameter
    */
  def search(tpe: TypeRepr): ImplicitSearchResult =
    ImplicitSearchResult.wrap(quotes.reflect.Implicits.search(tpe.unwrapWithin))

  /**
    * Find a given instance of type `T` in the current scope provided by the current enclosing splice,
    *  while excluding certain symbols from the initial implicit search.
    *  Return an `ImplicitSearchResult`.
    *
    *  @param tpe type of the implicit parameter
    *  @param ignored Symbols ignored during the initial implicit search
    *
    *  @note if an found given requires additional search for other given instances,
    *  this additional search will NOT exclude the symbols from the `ignored` list.
    */
  def searchIgnoring(tpe: TypeRepr)(ignored: Symbol*): ImplicitSearchResult =
    ImplicitSearchResult.wrap(quotes.reflect.Implicits.searchIgnoring(tpe.unwrapWithin)(ignored.map(_.unwrapWithin)*))

  // =====| Added |=====

  def searchOption[A: Type]: Option[Expr[A]] =
    search(TypeRepr.of[A]) match
      case ImplicitSearchSuccess(tree) => Some(tree.asExprOf[A])
      case ImplicitSearchFailure(_)    => None

  private def noGivenInstanceFound[A: Type]: String =
    s"No given instance found for ${TypeRepr.of[A].showAnsiCode}"

  def searchRequired[A: Type]: Expr[A] =
    search(TypeRepr.of[A]) match
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[A]
      case ImplicitSearchFailure(explanation) => report.errorAndAbort(explanation)
  def searchRequired[A: Type](pos: Position): Expr[A] =
    search(TypeRepr.of[A]) match
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[A]
      case ImplicitSearchFailure(explanation) => report.errorAndAbort(explanation, pos)
  def searchRequired[A: Type](missingMessage: => String): Expr[A] =
    search(TypeRepr.of[A]) match
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[A]
      case ImplicitSearchFailure(explanation) => report.errorAndAbort(s"${noGivenInstanceFound[A]}\n\n$missingMessage\n\n$explanation")
  def searchRequired[A: Type](missingMessage: => String, pos: Position): Expr[A] =
    search(TypeRepr.of[A]) match
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[A]
      case ImplicitSearchFailure(explanation) => report.errorAndAbort(s"${noGivenInstanceFound[A]}\n\n$missingMessage\n\n$explanation", pos)

  // ImplicitSearchFailure yielding some severely useless messages...
  def searchRequiredIgnoreExplanation[A: Type]: Expr[A] =
    Expr.summon[A].getOrElse { report.errorAndAbort(noGivenInstanceFound[A]) }
  def searchRequiredIgnoreExplanation[A: Type](pos: Position): Expr[A] =
    Expr.summon[A].getOrElse { report.errorAndAbort(noGivenInstanceFound[A], pos) }
  def searchRequiredIgnoreExplanation[A: Type](missingMessage: => String): Expr[A] =
    Expr.summon[A].getOrElse { report.errorAndAbort(s"${noGivenInstanceFound[A]}\n\n$missingMessage") }
  def searchRequiredIgnoreExplanation[A: Type](missingMessage: => String, pos: Position): Expr[A] =
    Expr.summon[A].getOrElse { report.errorAndAbort(s"${noGivenInstanceFound[A]}\n\n$missingMessage", pos) }

  // ImplicitSearchFailure yielding some severely useless messages...
  @deprecated("use 'searchRequiredIgnoreExplanation'", "0.0.93")
  def searchRequiredIgnoreMessage[A: Type]: Expr[A] = searchRequiredIgnoreExplanation[A]
  @deprecated("use 'searchRequiredIgnoreExplanation'", "0.0.93")
  def searchRequiredIgnoreMessage[A: Type](pos: Position): Expr[A] = searchRequiredIgnoreExplanation[A](pos)

  def searchIgnoringOption[A: Type](ignored: Symbol*): Option[Expr[A]] =
    searchIgnoring(TypeRepr.of[A])(ignored*) match
      case ImplicitSearchSuccess(tree) => Some(tree.asExprOf[A])
      case ImplicitSearchFailure(_)    => None

  def searchIgnoringRequired[A: Type](ignored: Symbol*): Expr[A] =
    searchIgnoring(TypeRepr.of[A])(ignored*) match
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[A]
      case ImplicitSearchFailure(explanation) => report.errorAndAbort(explanation)

}
