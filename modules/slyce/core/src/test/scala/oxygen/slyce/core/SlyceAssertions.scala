package oxygen.slyce.core

import oxygen.predef.test.*

object SlyceAssertions {

  object sourcePosition {

    private def make[A](name: String, exp: A, f: PartialFunction[SourcePosition, A]): Assertion[SourcePosition] =
      equalTo(exp).cmap[SourcePosition](name)(f)

    def hasAbsolutePos(absolutePos: Int): Assertion[SourcePosition] =
      make("absolutePos", absolutePos, _.absolutePos)

    def hasLineNo(lineNo: Int): Assertion[SourcePosition] =
      make("lineNo", lineNo, _.lineNo)

    def hasPosInLine(posInLine: Int): Assertion[SourcePosition] =
      make("posInLine", posInLine, _.posInLine)

    def hasCurrentChar(char: Char): Assertion[SourcePosition] =
      make("currentChar", char, _.unsafeCurrentChar)

    def hasCurrentChar(char: Option[Char]): Assertion[SourcePosition] = char match
      case Some(char) => hasCurrentChar(char)
      case None       => isAtSourceEOF

    def hasLinePos(lineNo: Int, posInLine: Int): Assertion[SourcePosition] =
      hasLineNo(lineNo) && hasPosInLine(posInLine)

    def isFully(absolutePos: Int, lineNo: Int, posInLine: Int): Assertion[SourcePosition] =
      hasAbsolutePos(absolutePos) && hasLineNo(lineNo) && hasPosInLine(posInLine)

    def isAtSourceStart: Assertion[SourcePosition] =
      make("isAtSourceStart", true, _.isAtSourceStart)

    def isAtSourceLastChar: Assertion[SourcePosition] =
      make("isAtSourceLastChar", true, _.isAtSourceLastChar)

    def isAtSourceEOF: Assertion[SourcePosition] =
      make("isAtSourceEOF", true, _.isAtSourceEOF)

    def isAtLineStart: Assertion[SourcePosition] =
      make("isAtLineStart", true, _.isAtLineStart)

    def isFirstCharInLine: Assertion[SourcePosition] =
      make("isFirstCharInLine", true, _.isFirstCharInLine)

    def isLastCharInLine: Assertion[SourcePosition] =
      make("isLastCharInLine", true, _.isLastCharInLine)

    def isAtLineEnd: Assertion[SourcePosition] =
      make("isAtLineEnd", true, _.isAtLineEnd)

  }

}
