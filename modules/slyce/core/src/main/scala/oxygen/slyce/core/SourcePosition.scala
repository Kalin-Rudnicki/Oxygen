package oxygen.slyce.core

import oxygen.predef.core.*
import scala.annotation.tailrec

sealed trait SourcePosition {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Values
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val source: Source
  val absolutePos: Int
  val lineNo: Int
  val posInLine: Int

  final inline def absolutePosZeroBased: Int = absolutePos
  final inline def lineNoZeroBased: Int = lineNo
  final inline def posInLineZeroBased: Int = posInLine

  final inline def absolutePosOneBased: Int = absolutePos + 1
  final inline def lineNoOneBased: Int = lineNo + 1
  final inline def posInLineOneBased: Int = posInLine + 1

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      1?
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final inline def foldEOF[A](nonEOF: SourcePosition.NonEOF => A, eof: SourcePosition.EOF => A): A = this match
    case self: SourcePosition.NonEOF => nonEOF(self)
    case self: SourcePosition.EOF    => eof(self)

  final def toOption: Option[SourcePosition.NonEOF] = this.foldEOF(_.some, _ => None)
  final def toEither: Either[SourcePosition.EOF, SourcePosition.NonEOF] = this.foldEOF(_.asRight, _.asLeft)

  final def nextOption: Option[SourcePosition] = this.foldEOF(_.next.some, _ => None)

  final def unsafeCurrentChar: Char = this.foldEOF(_.currentChar, self => throw new RuntimeException(s"Attempted to call SourcePosition.unsafeCurrentChar on: $self"))
  final def unsafeNext: SourcePosition = this.foldEOF(_.next, self => throw new RuntimeException(s"Attempted to call SourcePosition.unsafeNext on: $self"))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      2?
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def isEOL: Boolean
  def isEOF: Boolean
  final def isNormalChar: Boolean = !isEOL
  final def isNonEOL: Boolean = !isEOL

  final def isAtSourceStart: Boolean = absolutePos == 0
  final def isAtSourceLastChar: Boolean = absolutePos != 0 && absolutePos == source.lastCharIdx
  final def isAtSourceEOF: Boolean = absolutePos == source.eofIdx

  final def isAtLineStart: Boolean = posInLine == 0
  final def isFirstCharInLine: Boolean = isNormalChar && isAtLineStart
  final def isLastCharInLine: Boolean = isNormalChar && unsafeNext.isEOL
  final def isAtLineEnd: Boolean = isEOL

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      3?
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // requires that [[this.absolutePos]] <= [[idx]], and [[idx]] <= [[source.length]]
  @tailrec
  private def atSourceIndexInternal(idx: Int): SourcePosition =
    if (absolutePos < idx) this.unsafeNext.atSourceIndexInternal(idx)
    else this

  final def atSourceIndex(idx: Int): SourcePosition =
    if (idx < 0) throw new RuntimeException(s"invalid: SourcePosition.atIndex($idx) [min = 0]")
    else if (idx > source.length) throw new RuntimeException(s"invalid: SourcePosition.atIndex($idx) [max = ${source.length}]") // `>` instead of `>=` to allow incrementing to EOF
    else if (idx < absolutePos) SourcePosition.start(source).atSourceIndexInternal(idx)
    else this.atSourceIndexInternal(idx)

  final def atSourceStart: SourcePosition = SourcePosition.start(source)
  final def atSourceLastChar: SourcePosition = if (source.isEmpty) this.atSourceStart else this.atSourceIndex(source.length - 1)
  final def atSourceEOF: SourcePosition = this.atSourceIndex(source.length)

  // will return an EOL char if that EOL char is the only char on that line
  final def currentLineFirstChar: SourcePosition =
    if (posInLine == 0) this
    else SourcePosition.wrap(source, absolutePos - posInLine, lineNo, 0)

  // will return an EOL char if that EOL char is the only char on that line
  final def currentLineLastNonEOLChar: SourcePosition = {
    val tmp: SourcePosition = this.currentLineEOL
    if (tmp.posInLine > 0) SourcePosition.wrap(tmp.source, tmp.absolutePos - 1, tmp.lineNo, tmp.posInLine - 1)
    else tmp
  }

  @tailrec
  final def currentLineEOL: SourcePosition =
    if (this.isEOL) this
    else this.unsafeNext.currentLineEOL

  // will return an EOF char if this current pos is in the last line
  final def nextLineFirstChar: SourcePosition = {
    val tmp: SourcePosition = this.currentLineEOL
    if (tmp.isEOF) tmp
    else SourcePosition.wrap(tmp.source, tmp.absolutePos + 1, tmp.lineNo + 1, 0)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Display
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : include options which show a tag like EOF and/or EOL

  final def showLinePosZeroBased: String = s"$lineNoZeroBased:$posInLineZeroBased"
  final def showLinePosOneBased: String = s"$lineNoOneBased:$posInLineOneBased"

  final def showFullZeroBased: String = s"$absolutePosZeroBased @ $lineNoZeroBased:$posInLineZeroBased"
  final def showFullOneBased: String = s"$absolutePosOneBased @ $lineNoOneBased:$posInLineOneBased"

  // format: off
  final def show(includeAbsolute: Boolean, zeroBased: Boolean): String =
    if (includeAbsolute)
      if (zeroBased) showFullZeroBased
      else showFullOneBased
    else
      if (zeroBased) showLinePosZeroBased
      else showLinePosOneBased
  // format: on

  override final def toString: String = showFullOneBased

}
object SourcePosition {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Functions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def start(source: Source): SourcePosition = wrap(source, 0, 0, 0)
  def atIndex(source: Source, idx: Int): SourcePosition = SourcePosition.start(source).atSourceIndex(idx)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NonEOF extends SourcePosition {

    val currentChar: Char
    lazy val next: SourcePosition

  }

  final case class NonEOL private[SourcePosition] (
      source: Source,
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
      currentChar: Char,
  ) extends SourcePosition.NonEOF {

    override lazy val next: SourcePosition = SourcePosition.wrap(source, absolutePos + 1, lineNo, posInLine + 1)

    override def isEOL: Boolean = false
    override def isEOF: Boolean = false

  }

  final case class EOL private[SourcePosition] (
      source: Source,
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ) extends SourcePosition.NonEOF {

    override val currentChar: Char = '\n'

    override lazy val next: SourcePosition = SourcePosition.wrap(source, absolutePos + 1, lineNo + 1, 0)

    override def isEOL: Boolean = true
    override def isEOF: Boolean = false

  }

  final case class EOF private[SourcePosition] (
      source: Source,
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ) extends SourcePosition {

    override def isEOL: Boolean = true
    override def isEOF: Boolean = true

  }

  private def wrap(
      source: Source,
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ): SourcePosition =
    if (absolutePos == source.eofIdx) SourcePosition.EOF(source, absolutePos, lineNo, posInLine)
    else
      source.chars(absolutePos) match
        case '\n' => SourcePosition.EOL(source, absolutePos, lineNo, posInLine)
        case c    => SourcePosition.NonEOL(source, absolutePos, lineNo, posInLine, c)

  // FIX-PRE-MERGE (KR) :

}
