package oxygen.quoted.companion

import oxygen.quoted.*
import scala.annotation.{experimental, unused}
import scala.quoted.*

final class SelectorCompanion(using @unused quotes: Quotes) {}

final class SimpleSelectorCompanion(using quotes: Quotes) {

  @experimental
  def apply(name: String): SimpleSelector =
    SimpleSelector.wrap(quotes.reflect.SimpleSelector.apply(name))

}

final class RenameSelectorCompanion(using quotes: Quotes) {

  @experimental
  def apply(fromName: String, toName: String): RenameSelector =
    RenameSelector.wrap(quotes.reflect.RenameSelector.apply(fromName, toName))

}

final class OmitSelectorCompanion(using quotes: Quotes) {

  @experimental
  def apply(name: String): OmitSelector =
    OmitSelector.wrap(quotes.reflect.OmitSelector.apply(name))

}

final class GivenSelectorCompanion(using quotes: Quotes) {

  @experimental
  def apply(bound: Option[TypeTree]): GivenSelector =
    GivenSelector.wrap(quotes.reflect.GivenSelector.apply(bound.map(_.unwrapWithin)))

}
