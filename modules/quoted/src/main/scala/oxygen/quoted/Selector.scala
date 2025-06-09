package oxygen.quoted

import oxygen.quoted.companion.*
import scala.annotation.experimental
import scala.quoted.*

sealed trait Selector {
  type This <: Selector
  val quotes: Quotes
  val unwrap: quotes.reflect.Selector
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Selector = unwrap.asInstanceOf[newQuotes.reflect.Selector]
  given givenQuotes: quotes.type = quotes
}
object Selector {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Selector): Selector = unwrap match
    case unwrap: quotes.reflect.SimpleSelector => SimpleSelector.wrap(unwrap)
    case unwrap: quotes.reflect.RenameSelector => RenameSelector.wrap(unwrap)
    case unwrap: quotes.reflect.OmitSelector   => OmitSelector.wrap(unwrap)
    case unwrap: quotes.reflect.GivenSelector  => GivenSelector.wrap(unwrap)
    case _                                     => throw oxygen.quoted.error.UnknownCase("Selector", unwrap)

  def companion(using quotes: Quotes): SelectorCompanion = SelectorCompanion(using quotes)
  given Quotes => Conversion[Selector.type, SelectorCompanion] = _.companion

}

final class SimpleSelector(val quotes: Quotes)(val unwrap: quotes.reflect.SimpleSelector) extends Selector {
  override type This <: SimpleSelector
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.SimpleSelector = unwrap.asInstanceOf[newQuotes.reflect.SimpleSelector]

  def name: String = this.unwrap.name

  def namePos: Position = Position.wrap(this.unwrap.namePos)

}
object SimpleSelector {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.SimpleSelector): SimpleSelector =
    new SimpleSelector(quotes)(unwrap)

  def companion(using quotes: Quotes): SimpleSelectorCompanion = SimpleSelectorCompanion(using quotes)
  given Quotes => Conversion[SimpleSelector.type, SimpleSelectorCompanion] = _.companion

  @experimental
  def unapply(x: SimpleSelector): Some[String] = {
    given q: Quotes = x.quotes
    q.reflect.SimpleSelector.unapply(x.unwrapWithin)
  }

}

final class RenameSelector(val quotes: Quotes)(val unwrap: quotes.reflect.RenameSelector) extends Selector {
  override type This <: RenameSelector
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.RenameSelector = unwrap.asInstanceOf[newQuotes.reflect.RenameSelector]

  def fromName: String = this.unwrap.fromName

  def fromPos: Position = Position.wrap(this.unwrap.fromPos)

  def toName: String = this.unwrap.toName

  def toPos: Position = Position.wrap(this.unwrap.toPos)

}
object RenameSelector {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.RenameSelector): RenameSelector =
    new RenameSelector(quotes)(unwrap)

  def companion(using quotes: Quotes): RenameSelectorCompanion = RenameSelectorCompanion(using quotes)
  given Quotes => Conversion[RenameSelector.type, RenameSelectorCompanion] = _.companion

  @experimental
  def unapply(x: RenameSelector): (String, String) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.RenameSelector.unapply(x.unwrapWithin)
    (unwrap._1, unwrap._2)
  }

}

final class OmitSelector(val quotes: Quotes)(val unwrap: quotes.reflect.OmitSelector) extends Selector {
  override type This <: OmitSelector
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.OmitSelector = unwrap.asInstanceOf[newQuotes.reflect.OmitSelector]

  def name: String = this.unwrap.name

  def namePos: Position = Position.wrap(this.unwrap.namePos)

}
object OmitSelector {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.OmitSelector): OmitSelector =
    new OmitSelector(quotes)(unwrap)

  def companion(using quotes: Quotes): OmitSelectorCompanion = OmitSelectorCompanion(using quotes)
  given Quotes => Conversion[OmitSelector.type, OmitSelectorCompanion] = _.companion

  @experimental
  def unapply(x: OmitSelector): Some[String] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.OmitSelector.unapply(x.unwrapWithin)
    Some(unwrap.value)
  }

}

final class GivenSelector(val quotes: Quotes)(val unwrap: quotes.reflect.GivenSelector) extends Selector {
  override type This <: GivenSelector
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.GivenSelector = unwrap.asInstanceOf[newQuotes.reflect.GivenSelector]

  def bound: Option[TypeTree] = this.unwrap.bound.map(TypeTree.wrap(_))

}
object GivenSelector {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.GivenSelector): GivenSelector =
    new GivenSelector(quotes)(unwrap)

  def companion(using quotes: Quotes): GivenSelectorCompanion = GivenSelectorCompanion(using quotes)
  given Quotes => Conversion[GivenSelector.type, GivenSelectorCompanion] = _.companion

  @experimental
  def unapply(x: GivenSelector): Some[Option[TypeTree]] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.GivenSelector.unapply(x.unwrapWithin)
    Some(unwrap.value.map(TypeTree.wrap(_)))
  }

}
