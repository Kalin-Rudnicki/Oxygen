package oxygen.quoted.companion

import oxygen.quoted.*
import scala.quoted.*

final class PositionCompanion(using quotes: Quotes) {

  /** Position of the expansion site of the macro */
  def ofMacroExpansion: Position =
    Position.wrap(quotes.reflect.Position.ofMacroExpansion)

  /** Create a new position in the source with the given range. The range must be contained in the file. */
  def apply(sourceFile: SourceFile, start: Int, end: Int): Position =
    Position.wrap(quotes.reflect.Position.apply(sourceFile.unwrapWithin, start, end))

}
