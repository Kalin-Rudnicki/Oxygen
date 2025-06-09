package oxygen.quoted

import oxygen.quoted.companion.*
import scala.quoted.*

final class SourceFile private (using val quotes: Quotes)(val unwrap: quotes.reflect.SourceFile) {
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.SourceFile = this.unwrap.asInstanceOf[newQuotes.reflect.SourceFile]

  /** Path to this source file. May be `None` for virtual files such as in the REPL. */
  def getJPath: Option[java.nio.file.Path] = this.unwrap.getJPath

  /** Name of the source file */
  def name: String = this.unwrap.name

  /**
    * Path of the source file.
    *
    *  It does not necessarily point to a path in the filesystem, it could be the path of a virtual file.
    *  Use `getJPath` to get paths to the filesystem.
    */
  def path: String = this.unwrap.path

  /** Content of this source file */
  def content: Option[String] = this.unwrap.content

}
object SourceFile {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.SourceFile): SourceFile = SourceFile(unwrap)

  def companion(using quotes: Quotes): SourceFileCompanion = SourceFileCompanion(using quotes)
  given Quotes => Conversion[SourceFile.type, SourceFileCompanion] = _.companion

}
