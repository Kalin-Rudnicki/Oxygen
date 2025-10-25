package oxygen.slyce.core

import oxygen.predef.test.*

object SlyceAssertions {

  object sourcePosition {

    def hasAbsolutePos(absolutePos: Int): Assertion[SourcePosition] =
      equalTo(absolutePos).cmap[SourcePosition]("absolutePos")(_.absolutePos)

    def hasLineNo(lineNo: Int): Assertion[SourcePosition] =
      equalTo(lineNo).cmap[SourcePosition]("lineNo")(_.lineNo)

    def hasPosInLine(posInLine: Int): Assertion[SourcePosition] =
      equalTo(posInLine).cmap[SourcePosition]("posInLine")(_.posInLine)

    def hasCurrentChar(char: Char): Assertion[SourcePosition] =
      equalTo(char).cmap[SourcePosition]("currentChar")(_.unsafeCurrentChar)

    def isAtEOF: Assertion[SourcePosition] =
      equalTo(true).cmap[SourcePosition]("isAtEOF")(_.isAtEOF)

    def isAtFinalChar: Assertion[SourcePosition] =
      equalTo(true).cmap[SourcePosition]("isAtFinalChar")(_.isAtFinalChar)

    def hasCurrentChar(char: Option[Char]): Assertion[SourcePosition] = char match
      case Some(char) => hasCurrentChar(char)
      case None       => isAtEOF

    def hasLinePos(lineNo: Int, posInLine: Int): Assertion[SourcePosition] =
      hasLineNo(lineNo) && hasPosInLine(posInLine)

    def isFully(absolutePos: Int, lineNo: Int, posInLine: Int): Assertion[SourcePosition] =
      hasAbsolutePos(absolutePos) && hasLineNo(lineNo) && hasPosInLine(posInLine)

  }

}
