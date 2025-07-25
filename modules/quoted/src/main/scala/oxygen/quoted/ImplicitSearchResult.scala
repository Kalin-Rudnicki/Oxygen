package oxygen.quoted

import oxygen.quoted.error.UnknownCase
import scala.quoted.*

sealed trait ImplicitSearchResult extends Model {
  type This <: ImplicitSearchResult
  val quotes: Quotes
  val unwrap: quotes.reflect.ImplicitSearchResult
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ImplicitSearchResult = unwrap.asInstanceOf[newQuotes.reflect.ImplicitSearchResult]
  given givenQuotes: quotes.type = quotes

}
object ImplicitSearchResult {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ImplicitSearchResult): ImplicitSearchResult = unwrap match
    case unwrap: quotes.reflect.ImplicitSearchSuccess => ImplicitSearchSuccess.wrap(unwrap)
    case unwrap: quotes.reflect.ImplicitSearchFailure => ImplicitSearchFailure.wrap(unwrap)
    case _                                            => throw UnknownCase("ImplicitSearchResult", unwrap)

}

final class ImplicitSearchSuccess(val quotes: Quotes)(val unwrap: quotes.reflect.ImplicitSearchSuccess) extends ImplicitSearchResult {
  override type This <: ImplicitSearchSuccess
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ImplicitSearchSuccess = unwrap.asInstanceOf[newQuotes.reflect.ImplicitSearchSuccess]

  def tree: Tree = Tree.wrap(this.unwrap.tree)

}
object ImplicitSearchSuccess {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ImplicitSearchSuccess): ImplicitSearchSuccess =
    new ImplicitSearchSuccess(quotes)(unwrap)

  def unapply(x: ImplicitSearchSuccess): Some[Tree] =
    Some(x.tree)

}

sealed trait ImplicitSearchFailure extends ImplicitSearchResult {
  type This <: ImplicitSearchFailure
  override val unwrap: quotes.reflect.ImplicitSearchFailure
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ImplicitSearchFailure = unwrap.asInstanceOf[newQuotes.reflect.ImplicitSearchFailure]

  final def explanation: String = this.unwrap.explanation

}
object ImplicitSearchFailure {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ImplicitSearchFailure): ImplicitSearchFailure =
    unwrap match
      case unwrap: quotes.reflect.DivergingImplicit   => DivergingImplicit.wrap(unwrap)
      case unwrap: quotes.reflect.NoMatchingImplicits => NoMatchingImplicits.wrap(unwrap)
      case unwrap: quotes.reflect.AmbiguousImplicits  => AmbiguousImplicits.wrap(unwrap)
      case _                                          => GenericImplicitSearchFailure.wrap(unwrap)

  def unapply(x: ImplicitSearchFailure): Some[String] =
    Some(x.explanation)

}

final class DivergingImplicit(val quotes: Quotes)(val unwrap: quotes.reflect.DivergingImplicit) extends ImplicitSearchFailure {
  override type This <: DivergingImplicit
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.DivergingImplicit = unwrap.asInstanceOf[newQuotes.reflect.DivergingImplicit]
}
object DivergingImplicit {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.DivergingImplicit): DivergingImplicit =
    new DivergingImplicit(quotes)(unwrap)

}

final class NoMatchingImplicits(val quotes: Quotes)(val unwrap: quotes.reflect.NoMatchingImplicits) extends ImplicitSearchFailure {
  override type This <: NoMatchingImplicits
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.NoMatchingImplicits = unwrap.asInstanceOf[newQuotes.reflect.NoMatchingImplicits]
}
object NoMatchingImplicits {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.NoMatchingImplicits): NoMatchingImplicits =
    new NoMatchingImplicits(quotes)(unwrap)

}

final class AmbiguousImplicits(val quotes: Quotes)(val unwrap: quotes.reflect.AmbiguousImplicits) extends ImplicitSearchFailure {
  override type This <: AmbiguousImplicits
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.AmbiguousImplicits = unwrap.asInstanceOf[newQuotes.reflect.AmbiguousImplicits]
}
object AmbiguousImplicits {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.AmbiguousImplicits): AmbiguousImplicits =
    new AmbiguousImplicits(quotes)(unwrap)

}

final class GenericImplicitSearchFailure(val quotes: Quotes)(val unwrap: quotes.reflect.ImplicitSearchFailure) extends ImplicitSearchFailure {
  override type This <: ImplicitSearchFailure
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ImplicitSearchFailure = unwrap.asInstanceOf[newQuotes.reflect.ImplicitSearchFailure]
}
object GenericImplicitSearchFailure {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ImplicitSearchFailure): GenericImplicitSearchFailure =
    new GenericImplicitSearchFailure(quotes)(unwrap)

}
