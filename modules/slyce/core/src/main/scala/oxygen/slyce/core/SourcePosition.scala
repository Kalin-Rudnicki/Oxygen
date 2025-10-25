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

  final inline def absolutePosZeroIndexed: Int = absolutePos
  final inline def lineNoZeroIndexed: Int = lineNo
  final inline def posInLineZeroIndexed: Int = posInLine

  final inline def absolutePosOneIndexed: Int = absolutePos + 1
  final inline def lineNoOneIndexed: Int = lineNo + 1
  final inline def posInLineOneIndexed: Int = posInLine + 1

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      1?
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final inline def foldEOF[A](nonEOF: SourcePosition.NonEOF => A, eof: SourcePosition.EOF => A): A = this match
    case self: SourcePosition.NonEOF => nonEOF(self)
    case self: SourcePosition.EOF    => eof(self)

  final def toNonEOFOption: Option[SourcePosition.NonEOF] = this.foldEOF(_.some, _ => None)
  final def toNonEOFEither: Either[SourcePosition.EOF, SourcePosition.NonEOF] = this.foldEOF(_.asRight, _.asLeft)

  final inline def foldEOL[A](nonEOL: SourcePosition.NonEOL => A, eol: SourcePosition.EOL => A): A = this match
    case self: SourcePosition.NonEOL => nonEOL(self)
    case self: SourcePosition.EOL    => eol(self)

  final def toNonEOLOption: Option[SourcePosition.NonEOL] = this.foldEOL(_.some, _ => None)
  final def toNonEOLEither: Either[SourcePosition.EOL, SourcePosition.NonEOL] = this.foldEOL(_.asRight, _.asLeft)

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

  final def line: SourceLine = SourceLine(this)

  // requires that [[this.absolutePos]] <= [[idx]], and [[idx]] <= [[source.length]]
  @tailrec
  private def atSourceIndexInternal(idx: Int): SourcePosition =
    if (absolutePos < idx) this.unsafeNext.atSourceIndexInternal(idx)
    else this

  final def atSourceIndexZeroIndexed(idx: Int): SourcePosition =
    if (idx < 0) throw new RuntimeException(s"invalid: SourcePosition.atIndex($idx) [min = 0]")
    else if (idx > source.length) throw new RuntimeException(s"invalid: SourcePosition.atIndex($idx) [max = ${source.length}]") // `>` instead of `>=` to allow incrementing to EOF
    else if (idx < absolutePos) SourcePosition.start(source).atSourceIndexInternal(idx)
    else this.atSourceIndexInternal(idx)
  final def atSourceIndexOneIndexed(idx: Int): SourcePosition =
    this.atSourceIndexZeroIndexed(idx - 1)

  final def atSourceStart: SourcePosition = SourcePosition.start(source)
  final def atSourceLastChar: SourcePosition = if (source.isEmpty) this.atSourceStart else this.atSourceIndexZeroIndexed(source.length - 1)
  final def atSourceEOF: SourcePosition = this.atSourceIndexZeroIndexed(source.length)

  def unsafePrevLineEOL: SourcePosition.EOL =
    prevLineEOL.getOrElse { throw new RuntimeException(s"invalid: SourcePosition.unsafePrevLineEOL, current = $this") }

  final def prevLineEOL: Option[SourcePosition.EOL] = {
    val res: Option[SourcePosition] =
      if (lineNo == 0)
        None
      else if (lineNo == 1) {
        val start: SourcePosition = this.currentLineFirstChar
        SourcePosition.wrap(start.source, start.absolutePos - 1, start.lineNo - 1, start.absolutePos - 1).some
      } else {
        val start: SourcePosition = this.currentLineFirstChar
        var tmp: Int = start.absolutePos - 2

        while (source.chars(tmp) != '\n')
          tmp -= 1

        SourcePosition.wrap(start.source, start.absolutePos - 1, start.lineNo - 1, start.absolutePos - tmp - 2).some
      }

    res.map(_.foldEOL(self => throw new RuntimeException(s"internal defect: SourcePosition.unsafePrevLineEOL returned non-EOL: $self"), identity))
  }

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
  final def currentLineEOL: SourcePosition.EOL = this match
    case self: SourcePosition.NonEOL => self.next.currentLineEOL
    case self: SourcePosition.EOL    => self

  final def unsafeNextLineFirstChar: SourcePosition =
    this.nextLineFirstChar.getOrElse { throw new RuntimeException(s"invalid: SourcePosition.unsafeNextLineFirstChar, current = $this") }
  final def nextLineFirstChar: Option[SourcePosition] =
    this.currentLineEOL match
      case pos: SourcePosition.NonEOF => SourcePosition.wrap(pos.source, pos.absolutePos + 1, pos.lineNo + 1, 0).some
      case _: SourcePosition.EOF      => None

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Display
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : include options which show a tag like EOF and/or EOL

  final def showLinePosZeroIndexed: String = s"$lineNoZeroIndexed:$posInLineZeroIndexed"
  final def showLinePosOneIndexed: String = s"$lineNoOneIndexed:$posInLineOneIndexed"

  final def showFullZeroIndexed: String = s"$absolutePosZeroIndexed @ $lineNoZeroIndexed:$posInLineZeroIndexed"
  final def showFullOneIndexed: String = s"$absolutePosOneIndexed @ $lineNoOneIndexed:$posInLineOneIndexed"

  private def showCurrentChar: String = this.foldEOF(_.currentChar.unesc, _ => "EOF")
  final def showExtraZeroIndexed: String = s"$absolutePosZeroIndexed @ $lineNoZeroIndexed:$posInLineZeroIndexed ($showCurrentChar)"
  final def showExtraOneIndexed: String = s"$absolutePosOneIndexed @ $lineNoOneIndexed:$posInLineOneIndexed ($showCurrentChar)"

  // format: off
  final def show(includeAbsolute: Boolean, zeroIndexed: Boolean): String =
    if (includeAbsolute)
      if (zeroIndexed) showFullZeroIndexed
      else showFullOneIndexed
    else
      if (zeroIndexed) showLinePosZeroIndexed
      else showLinePosOneIndexed
  // format: on

  override final def toString: String = showFullOneIndexed

}
object SourcePosition {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def start(source: Source): SourcePosition = wrap(source, 0, 0, 0)

  // TODO (KR) : optional version of this

  def atSourceIndexZeroIndexed(source: Source, idx: Int): SourcePosition =
    SourcePosition.start(source).atSourceIndexZeroIndexed(idx)
  def atSourceIndexOneIndexed(source: Source, idx: Int): SourcePosition =
    SourcePosition.start(source).atSourceIndexOneIndexed(idx)
  def atSourceIndex(source: Source, idx: Int): SourcePosition =
    SourcePosition.atSourceIndexZeroIndexed(source, idx)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NonEOF extends SourcePosition {

    override final def isEOF: Boolean = false

    val currentChar: Char
    lazy val next: SourcePosition

  }

  sealed trait EOL extends SourcePosition {
    override final def isEOL: Boolean = true
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

  }

  final case class NewLine private[SourcePosition] (
      source: Source,
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ) extends SourcePosition.NonEOF,
        SourcePosition.EOL {

    override val currentChar: Char = '\n'

    override lazy val next: SourcePosition = SourcePosition.wrap(source, absolutePos + 1, lineNo + 1, 0)

  }

  final case class EOF private[SourcePosition] (
      source: Source,
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ) extends SourcePosition.EOL {

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
        case '\n' => SourcePosition.NewLine(source, absolutePos, lineNo, posInLine)
        case c    => SourcePosition.NonEOL(source, absolutePos, lineNo, posInLine, c)

  // FIX-PRE-MERGE (KR) :

}
