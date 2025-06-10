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

  def searchRequired[A: Type]: Expr[A] =
    search(TypeRepr.of[A]) match
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[A]
      case ImplicitSearchFailure(explanation) => report.errorAndAbort(explanation)

  def searchIgnoringOption[A: Type](ignored: Symbol*): Option[Expr[A]] =
    searchIgnoring(TypeRepr.of[A])(ignored*) match
      case ImplicitSearchSuccess(tree) => Some(tree.asExprOf[A])
      case ImplicitSearchFailure(_)    => None

  def searchIgnoringRequired[A: Type](ignored: Symbol*): Expr[A] =
    searchIgnoring(TypeRepr.of[A])(ignored*) match
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[A]
      case ImplicitSearchFailure(explanation) => report.errorAndAbort(explanation)

}
