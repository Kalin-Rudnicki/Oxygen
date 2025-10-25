package oxygen.slyce.core

import scala.annotation.tailrec

/**
  * Represents an index in a file.
  * [[absolutePos]], [[lineNo]], and [[posInLine]] are all zero-based.
  */
final case class SourcePosition private (
    source: Source,
    absolutePos: Int,
    lineNo: Int,
    posInLine: Int,
) {

  inline def absolutePosZeroBased: Int = absolutePos
  inline def lineNoZeroBased: Int = lineNo
  inline def posInLineZeroBased: Int = posInLine

  inline def absolutePosOneBased: Int = absolutePos + 1
  inline def lineNoOneBased: Int = lineNo + 1
  inline def posInLineOneBased: Int = posInLine + 1

  inline def canRead: Boolean = absolutePos < source.length

  def isAtFinalChar: Boolean = absolutePos + 1 == source.length
  def isAtEOF: Boolean = absolutePos >= source.length

  private def internalIncrementUntil(idx: Int): SourcePosition = {
    var tmp: SourcePosition = this
    while (tmp.absolutePos < idx)
      tmp = tmp.unsafeReadAndNextNoChecks._2

    tmp
  }

  inline def unsafeCurrentChar: Char = source(absolutePos)

  def positionType: SourcePosition.PositionType = absolutePos match
    case source.eofIdx       => SourcePosition.PositionType.EOF
    case source.finalCharIdx => SourcePosition.PositionType.FinalChar
    case _                   => SourcePosition.PositionType.Middle

  // TODO (KR) : have different versions like `readAndNext`
  @tailrec
  def atIndex(idx: Int): SourcePosition =
    if (idx < 0) throw new RuntimeException(s"invalid: SourcePosition.atIndex($idx) [min = 0]")
    else if (idx == absolutePos) this
    else if (idx < absolutePos) SourcePosition.start(source).atIndex(idx)
    else if (idx > source.length) throw new RuntimeException(s"invalid: SourcePosition.atIndex($idx) [max = ${source.length}]") // `>` instead of `>=` to allow incrementing to EOF
    else this.internalIncrementUntil(idx)

  def atStartOfCurrentLine: SourcePosition =
    SourcePosition(source, absolutePos - posInLine, lineNo, 0)

  def atEndOfCurrentLineBeforeNewLine: SourcePosition = {
    val tmp: SourcePosition = this.atEndOfCurrentLineOnNewLine
    if (tmp.posInLine > 0) SourcePosition(tmp.source, tmp.absolutePos - 1, tmp.lineNo, tmp.posInLine - 1)
    else tmp
  }

  def atEndOfCurrentLineOnNewLine: SourcePosition = {
    var current: SourcePosition = this
    while (!current.isAtEOF) {
      val (currentChar, next) = current.unsafeReadAndNextNoChecks
      currentChar match
        case '\n' => return current
        case _    => current = next
    }

    current
  }

  def atStartOfNextLine: SourcePosition = {
    val tmp: SourcePosition = this.atEndOfCurrentLineOnNewLine
    if (tmp.isAtEOF) tmp
    else SourcePosition(tmp.source, tmp.absolutePos + 1, tmp.lineNo + 1, 0)
  }

  def atStart: SourcePosition = SourcePosition.start(source)
  def atLastChar: SourcePosition = if (source.isEmpty) this.atStart else this.atIndex(source.length - 1)
  def atEOF: SourcePosition = this.atIndex(source.length)

  /////// readAndNext ///////////////////////////////////////////////////////////////

  /**
    * Attempts to read current [[Char]] based on [[absolutePos]], compute next [[SourcePosition]], and return both.
    * This is done without any care for out of bounds-ness.
    */
  def unsafeReadAndNextNoChecks: (Char, SourcePosition) = {
    val current: Char = unsafeCurrentChar
    current match
      case '\n' => (current, SourcePosition(source, absolutePos + 1, lineNo + 1, 0))
      case _    => (current, SourcePosition(source, absolutePos + 1, lineNo, posInLine + 1))
  }

  /**
    * Attempts to read current [[Char]] based on [[absolutePos]], compute next [[SourcePosition]], and return both.
    * If the current [[SourcePosition]] is already EOF, returns [[None]].
    */
  def readAndNextOption: Option[(Char, SourcePosition)] =
    Option.when(this.canRead)(unsafeReadAndNextNoChecks)

  /**
    * Attempts to read current [[Char]] based on [[absolutePos]], compute next [[SourcePosition]], and return both.
    * If the current [[SourcePosition]] is already EOF, an exception will be thrown.
    */
  def unsafeReadAndNext: (Char, SourcePosition) =
    if (this.canRead) unsafeReadAndNextNoChecks
    else throw new IndexOutOfBoundsException(s"Attempted to read from source '${source.sourceType}' at index $absolutePos, when source only has a length of ${source.length}")

  /////// display ///////////////////////////////////////////////////////////////

  def showLinePosZeroBased: String = s"$lineNoZeroBased:$posInLineZeroBased"
  def showLinePosOneBased: String = s"$lineNoOneBased:$posInLineOneBased"

  def showFullZeroBased: String = s"$absolutePosZeroBased @ $lineNoZeroBased:$posInLineZeroBased"
  def showFullOneBased: String = s"$absolutePosOneBased @ $lineNoOneBased:$posInLineOneBased"
  
  // format: off
  def show(includeAbsolute: Boolean, zeroBased: Boolean): String =
    if (includeAbsolute)
      if (zeroBased) showFullZeroBased
      else showFullOneBased
    else
      if (zeroBased) showLinePosZeroBased
      else showLinePosOneBased
  // format: on

  override def toString: String = showFullOneBased

}
object SourcePosition {

  def start(source: Source): SourcePosition = SourcePosition(source, 0, 0, 0)

  def atIndex(source: Source, idx: Int): SourcePosition =
    SourcePosition.start(source).atIndex(idx)

  // TODO (KR) : improve this to represent things like `hasNextLine`, `hasPrevLine`
  enum PositionType {
    case EOF
    case FinalChar
    case Middle
  }

}
