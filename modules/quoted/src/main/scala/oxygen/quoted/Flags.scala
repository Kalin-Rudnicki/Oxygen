package oxygen.quoted

import oxygen.quoted.companion.*
import scala.quoted.*

final class Flags private (using val quotes: Quotes)(val unwrap: quotes.reflect.Flags) extends Model {
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Flags = this.unwrap.asInstanceOf[newQuotes.reflect.Flags]

  /** Is the given flag set a subset of this flag sets */
  def is(that: Flags): Boolean = this.unwrap.is(that.unwrapWithin)

  /** Union of the two flag sets */
  def |(that: Flags): Flags = Flags.wrap(this.unwrap | that.unwrapWithin)

  /** Intersection of the two flag sets */
  def &(that: Flags): Flags = Flags.wrap(this.unwrap & that.unwrapWithin)

  /** Shows the flags as a String */
  def show: String = this.unwrap.show

}
object Flags {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Flags): Flags = Flags(unwrap)

  def companion(using quotes: Quotes): FlagsCompanion = FlagsCompanion(using quotes)
  given Quotes => Conversion[Flags.type, FlagsCompanion] = _.companion

}
