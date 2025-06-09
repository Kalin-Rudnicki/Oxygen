package oxygen.quoted.companion

import oxygen.quoted.*
import scala.quoted.*

final class SourceFileCompanion(using quotes: Quotes) {

  /** Returns the source file being compiled. The path is relative to the current working directory. */
  def current: SourceFile =
    SourceFile.wrap(quotes.reflect.SourceFile.current)

}
