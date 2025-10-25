package oxygen.slyce.core

import oxygen.predef.core.*
import scala.annotation.tailrec

sealed trait SourceLine {

  val lineText: String
  val eol: SourcePosition.EOL

  final def source: Source = eol.source

  final def lineNoZeroIndexed: Int = eol.lineNoZeroIndexed
  final def lineNoOneIndexed: Int = eol.lineNoOneIndexed
  final def lineLength: Int = eol.posInLine

  final def isLastSourceLine: Boolean = eol.isEOF
  final def eolIsEOF: Boolean = eol.isEOF

  final def prevLine: Option[SourceLine] =
    eol.prevLineEOL.map(SourceLine(_))
  final def unsafePrevLine: SourceLine =
    this.prevLine.getOrElse { throw new RuntimeException(s"No prev line before line:\n$this") }

  final def nextLine: Option[SourceLine] = eol match
    case eol: SourcePosition.NewLine => SourceLine(eol.next).some
    case _: SourcePosition.EOF       => None
  final def unsafeNextLine: SourceLine =
    this.nextLine.getOrElse { throw new RuntimeException(s"No next line after line:\n$this") }

  override final def toString: String = s"${lineNoOneIndexed.toString.alignRight(6)} | $lineText"

}
object SourceLine {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(pos: SourcePosition): SourceLine =
    pos match {
      case pos: SourcePosition.EOL if pos.posInLine == 0 => SourceLine.Empty(pos)
      case _                                             =>
        val eol: SourcePosition.EOL = pos.currentLineEOL
        val first: SourcePosition.NonEOL = pos.currentLineFirstChar.toNonEOL("first")
        val last: SourcePosition.NonEOL = pos.currentLineLastNonEOLChar.toNonEOL("last")
        SourceLine.NonEmpty(first, last, pos.source.rawText.substring(first.absolutePos, eol.absolutePos), eol)
    }

  // TODO (KR) : optional version of this

  def atSourceLineNoZeroIndexed(source: Source, lineNo: Int): SourceLine = {
    @tailrec
    def loop(pos: SourcePosition): SourceLine =
      if (pos.lineNo == lineNo) SourceLine(pos)
      else loop(pos.unsafeNext)

    loop(SourcePosition.start(source))
  }
  def atSourceLineNoOneIndexed(source: Source, lineNo: Int): SourceLine =
    SourceLine.atSourceLineNoZeroIndexed(source, lineNo - 1)
  def atSourceLineNo(source: Source, lineNo: Int): SourceLine =
    SourceLine.atSourceLineNoZeroIndexed(source, lineNo)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Empty private[SourceLine] (
      eol: SourcePosition.EOL,
  ) extends SourceLine {
    override val lineText: String = ""
  }

  final case class NonEmpty private[SourceLine] (
      firstChar: SourcePosition.NonEOL,
      lastChar: SourcePosition.NonEOL,
      lineText: String,
      eol: SourcePosition.EOL,
  ) extends SourceLine

  extension (self: SourcePosition)
    private def toNonEOL(name: String): SourcePosition.NonEOL =
      self.foldEOL(identity, self => throw new RuntimeException(s"internal defect: non-empty line ($name) was not non-EOL: $self"))

}
