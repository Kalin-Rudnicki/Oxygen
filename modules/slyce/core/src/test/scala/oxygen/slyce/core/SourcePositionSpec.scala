package oxygen.slyce.core

import oxygen.predef.test.*
import oxygen.slyce.core.SlyceAssertions.sourcePosition.*
import zio.test.TestResult

object SourcePositionSpec extends OxygenSpecDefault {

  //                          0123 4567 89__  |  i=10, eof=11
  private val str1: String = "abc\ndef\nghi"

  //                          0123 4 5678
  private val str2: String = "abc\n\ndef"

  //                          0
  private val str3: String = ""

  private object relativePosition {

    private def relativePositionTest(rawText: String, initialIndex: Int)(
        at: Assertion[SourcePosition],
        currentLineFirstChar: Assertion[SourcePosition],
        currentLineLastNonEOLChar: Assertion[SourcePosition],
        currentLineEOL: Assertion[SourcePosition],
        nextLineFirstChar: Assertion[SourcePosition],
    )(using Trace, SourceLocation): TestSpec =
      test(s"initialIndex = $initialIndex") {
        val source: Source = Source.rawText(rawText)
        val init: SourcePosition = SourcePosition.atIndex(source, initialIndex)

        assert(init)(at.label("at")) &&
        assert(init.currentLineFirstChar)(currentLineFirstChar.label("currentLineFirstChar")) &&
        assert(init.currentLineLastNonEOLChar)(currentLineLastNonEOLChar.label("currentLineLastNonEOLChar")) &&
        assert(init.currentLineEOL)(currentLineEOL.label("currentLineEOL")) &&
        assert(init.nextLineFirstChar)(nextLineFirstChar.label("nextLineFirstChar"))
      }

    def spec: TestSpec =
      suite("relative position")(
        suite("str1")(
          relativePositionTest(str1, 0)(
            at = isFully(0, 0, 0) && hasCurrentChar('a'),
            currentLineFirstChar = isFully(0, 0, 0) && hasCurrentChar('a'),
            currentLineLastNonEOLChar = isFully(2, 0, 2) && hasCurrentChar('c'),
            currentLineEOL = isFully(3, 0, 3) && hasCurrentChar('\n'),
            nextLineFirstChar = isFully(4, 1, 0) && hasCurrentChar('d'),
          ),
          relativePositionTest(str1, 1)(
            at = isFully(1, 0, 1) && hasCurrentChar('b'),
            currentLineFirstChar = isFully(0, 0, 0) && hasCurrentChar('a'),
            currentLineLastNonEOLChar = isFully(2, 0, 2) && hasCurrentChar('c'),
            currentLineEOL = isFully(3, 0, 3) && hasCurrentChar('\n'),
            nextLineFirstChar = isFully(4, 1, 0) && hasCurrentChar('d'),
          ),
          relativePositionTest(str1, 3)(
            at = isFully(3, 0, 3) && hasCurrentChar('\n'),
            currentLineFirstChar = isFully(0, 0, 0) && hasCurrentChar('a'),
            currentLineLastNonEOLChar = isFully(2, 0, 2) && hasCurrentChar('c'),
            currentLineEOL = isFully(3, 0, 3) && hasCurrentChar('\n'),
            nextLineFirstChar = isFully(4, 1, 0) && hasCurrentChar('d'),
          ),
          relativePositionTest(str1, 5)(
            at = isFully(5, 1, 1) && hasCurrentChar('e'),
            currentLineFirstChar = isFully(4, 1, 0) && hasCurrentChar('d'),
            currentLineLastNonEOLChar = isFully(6, 1, 2) && hasCurrentChar('f'),
            currentLineEOL = isFully(7, 1, 3) && hasCurrentChar('\n'),
            nextLineFirstChar = isFully(8, 2, 0) && hasCurrentChar('g'),
          ),
          relativePositionTest(str1, 9)(
            at = isFully(9, 2, 1) && hasCurrentChar('h'),
            currentLineFirstChar = isFully(8, 2, 0) && hasCurrentChar('g'),
            currentLineLastNonEOLChar = isFully(10, 2, 2) && hasCurrentChar('i'),
            currentLineEOL = isFully(11, 2, 3) && isAtSourceEOF,
            nextLineFirstChar = isFully(11, 2, 3) && isAtSourceEOF,
          ),
          relativePositionTest(str1, 11)(
            at = isFully(11, 2, 3) && isAtSourceEOF,
            currentLineFirstChar = isFully(8, 2, 0) && hasCurrentChar('g'),
            currentLineLastNonEOLChar = isFully(10, 2, 2) && hasCurrentChar('i'),
            currentLineEOL = isFully(11, 2, 3) && isAtSourceEOF,
            nextLineFirstChar = isFully(11, 2, 3) && isAtSourceEOF,
          ),
        ),
        suite("str2")(
          relativePositionTest(str2, 3)(
            at = isFully(3, 0, 3) && hasCurrentChar('\n'),
            currentLineFirstChar = isFully(0, 0, 0) && hasCurrentChar('a'),
            currentLineLastNonEOLChar = isFully(2, 0, 2) && hasCurrentChar('c'),
            currentLineEOL = isFully(3, 0, 3) && hasCurrentChar('\n'),
            nextLineFirstChar = isFully(4, 1, 0) && hasCurrentChar('\n'),
          ),
          relativePositionTest(str2, 4)(
            at = isFully(4, 1, 0) && hasCurrentChar('\n'),
            currentLineFirstChar = isFully(4, 1, 0) && hasCurrentChar('\n'),
            currentLineLastNonEOLChar = isFully(4, 1, 0) && hasCurrentChar('\n'),
            currentLineEOL = isFully(4, 1, 0) && hasCurrentChar('\n'),
            nextLineFirstChar = isFully(5, 2, 0) && hasCurrentChar('d'),
          ),
        ),
        suite("str3")(
          relativePositionTest(str3, 0)(
            at = isFully(0, 0, 0) && isAtSourceEOF,
            currentLineFirstChar = isFully(0, 0, 0) && isAtSourceEOF,
            currentLineLastNonEOLChar = isFully(0, 0, 0) && isAtSourceEOF,
            currentLineEOL = isFully(0, 0, 0) && isAtSourceEOF,
            nextLineFirstChar = isFully(0, 0, 0) && isAtSourceEOF,
          ),
        ),
      )

  }

  private object positionAspects {

    def spec: TestSpec =
      suite("position aspects")(
        test("str1") {
          val source: Source = Source.rawText(str1)

          def make(idx: Int)(assertion: Assertion[SourcePosition]): TestResult =
            assert(source(idx))(assertion.label(s"idx = $idx"))

          make(0) { hasCurrentChar('a') && isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && isAtLineStart && isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(1) { hasCurrentChar('b') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(2) { hasCurrentChar('c') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && isLastCharInLine && !isAtLineEnd } &&
          make(3) { hasCurrentChar('\n') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && isAtLineEnd } &&
          make(4) { hasCurrentChar('d') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && isAtLineStart && isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(5) { hasCurrentChar('e') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(6) { hasCurrentChar('f') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && isLastCharInLine && !isAtLineEnd } &&
          make(7) { hasCurrentChar('\n') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && isAtLineEnd } &&
          make(8) { hasCurrentChar('g') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && isAtLineStart && isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(9) { hasCurrentChar('h') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(10) { hasCurrentChar('i') && !isAtSourceStart && isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && isLastCharInLine && !isAtLineEnd } &&
          make(11) { hasCurrentChar(None) && !isAtSourceStart && !isAtSourceLastChar && isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && isAtLineEnd }
        },
        test("str2") {
          val source: Source = Source.rawText(str2)

          def make(idx: Int)(assertion: Assertion[SourcePosition]): TestResult =
            assert(source(idx))(assertion.label(s"idx = $idx"))

          make(0) { hasCurrentChar('a') && isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && isAtLineStart && isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(1) { hasCurrentChar('b') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(2) { hasCurrentChar('c') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && isLastCharInLine && !isAtLineEnd } &&
          make(3) { hasCurrentChar('\n') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && isAtLineEnd } &&
          make(4) { hasCurrentChar('\n') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && isAtLineStart && !isFirstCharInLine && !isLastCharInLine && isAtLineEnd } &&
          make(5) { hasCurrentChar('d') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && isAtLineStart && isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(6) { hasCurrentChar('e') && !isAtSourceStart && !isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && !isAtLineEnd } &&
          make(7) { hasCurrentChar('f') && !isAtSourceStart && isAtSourceLastChar && !isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && isLastCharInLine && !isAtLineEnd } &&
          make(8) { hasCurrentChar(None) && !isAtSourceStart && !isAtSourceLastChar && isAtSourceEOF && !isAtLineStart && !isFirstCharInLine && !isLastCharInLine && isAtLineEnd }
        },
        test("str3") {
          val source: Source = Source.rawText(str3)

          def make(idx: Int)(assertion: Assertion[SourcePosition]): TestResult =
            assert(source(idx))(assertion.label(s"idx = $idx"))

          make(0) { hasCurrentChar(None) && isAtSourceStart && !isAtSourceLastChar && isAtSourceEOF && isAtLineStart && !isFirstCharInLine && !isLastCharInLine && isAtLineEnd }
        },
      )

  }

  override def testSpec: TestSpec =
    suite("SourcePositionSpec")(
      relativePosition.spec,
      positionAspects.spec,
    )

}
