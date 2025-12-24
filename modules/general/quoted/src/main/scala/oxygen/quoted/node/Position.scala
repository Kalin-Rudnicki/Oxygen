package oxygen.quoted.node

import oxygen.quoted.companion.*
import scala.quoted.*

final class Position private (using val quotes: Quotes)(val unwrap: quotes.reflect.Position) extends Model {
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Position = this.unwrap.asInstanceOf[newQuotes.reflect.Position]

  /** The start offset in the source file */
  def start: Int = this.unwrap.start

  /** The end offset in the source file */
  def end: Int = this.unwrap.end

  /** Source file in which this position is located */
  def sourceFile: SourceFile = SourceFile.wrap(this.unwrap.sourceFile)

  /** The start line in the source file */
  def startLine: Int = this.unwrap.startLine

  /** The end line in the source file */
  def endLine: Int = this.unwrap.endLine

  /** The start column in the source file */
  def startColumn: Int = this.unwrap.startColumn

  /** The end column in the source file */
  def endColumn: Int = this.unwrap.endColumn

  /** Source code within the position */
  def sourceCode: Option[String] = this.unwrap.sourceCode

  // =====| Added |=====

  override def maybePos: Option[Position] = Some(this)

}
object Position {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Position): Position = Position(unwrap)

  def companion(using quotes: Quotes): PositionCompanion = PositionCompanion(using quotes)
  given Quotes => Conversion[Position.type, PositionCompanion] = _.companion

}
