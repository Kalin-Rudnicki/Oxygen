package oxygen.slyce.core

import oxygen.predef.test.*
import oxygen.slyce.core.SlyceAssertions.sourcePosition.*

object SourceSpec extends OxygenSpecDefault {

  private def relativePositionTest(rawText: String, initialIndex: Int)(
      at: Assertion[SourcePosition],
      atStartOfCurrentLine: Assertion[SourcePosition],
      atEndOfCurrentLineBeforeNewLine: Assertion[SourcePosition],
      atEndOfCurrentLineOnNewLine: Assertion[SourcePosition],
      atStartOfNextLine: Assertion[SourcePosition],
  )(using Trace, SourceLocation): TestSpec =
    test(s"initialIndex = $initialIndex") {
      val source: Source = Source.rawText(rawText)
      val init: SourcePosition = SourcePosition.atIndex(source, initialIndex)

      assert(init)(at.label("at")) &&
      assert(init.atStartOfCurrentLine)(atStartOfCurrentLine.label("atStartOfCurrentLine")) &&
      assert(init.atEndOfCurrentLineBeforeNewLine)(atEndOfCurrentLineBeforeNewLine.label("atEndOfCurrentLineBeforeNewLine")) &&
      assert(init.atEndOfCurrentLineOnNewLine)(atEndOfCurrentLineOnNewLine.label("atEndOfCurrentLineOnNewLine")) &&
      assert(init.atStartOfNextLine)(atStartOfNextLine.label("atStartOfNextLine"))
    }

  //                          0123 4567 89__  |  i=10, eof=11
  private val str1: String = "abc\ndef\nghi"

  //                          0123 4 5678
  private val str2: String = "abc\n\ndef"

  //                          0
  private val str3: String = ""

  override def testSpec: TestSpec =
    suite("SourceSpec")(
      suite("relative position")(
        suite("str1")(
          relativePositionTest(str1, 0)(
            at = isFully(0, 0, 0) && hasCurrentChar('a'),
            atStartOfCurrentLine = isFully(0, 0, 0) && hasCurrentChar('a'),
            atEndOfCurrentLineBeforeNewLine = isFully(2, 0, 2) && hasCurrentChar('c'),
            atEndOfCurrentLineOnNewLine = isFully(3, 0, 3) && hasCurrentChar('\n'),
            atStartOfNextLine = isFully(4, 1, 0) && hasCurrentChar('d'),
          ),
          relativePositionTest(str1, 1)(
            at = isFully(1, 0, 1) && hasCurrentChar('b'),
            atStartOfCurrentLine = isFully(0, 0, 0) && hasCurrentChar('a'),
            atEndOfCurrentLineBeforeNewLine = isFully(2, 0, 2) && hasCurrentChar('c'),
            atEndOfCurrentLineOnNewLine = isFully(3, 0, 3) && hasCurrentChar('\n'),
            atStartOfNextLine = isFully(4, 1, 0) && hasCurrentChar('d'),
          ),
          relativePositionTest(str1, 3)(
            at = isFully(3, 0, 3) && hasCurrentChar('\n'),
            atStartOfCurrentLine = isFully(0, 0, 0) && hasCurrentChar('a'),
            atEndOfCurrentLineBeforeNewLine = isFully(2, 0, 2) && hasCurrentChar('c'),
            atEndOfCurrentLineOnNewLine = isFully(3, 0, 3) && hasCurrentChar('\n'),
            atStartOfNextLine = isFully(4, 1, 0) && hasCurrentChar('d'),
          ),
          relativePositionTest(str1, 5)(
            at = isFully(5, 1, 1) && hasCurrentChar('e'),
            atStartOfCurrentLine = isFully(4, 1, 0) && hasCurrentChar('d'),
            atEndOfCurrentLineBeforeNewLine = isFully(6, 1, 2) && hasCurrentChar('f'),
            atEndOfCurrentLineOnNewLine = isFully(7, 1, 3) && hasCurrentChar('\n'),
            atStartOfNextLine = isFully(8, 2, 0) && hasCurrentChar('g'),
          ),
          relativePositionTest(str1, 9)(
            at = isFully(9, 2, 1) && hasCurrentChar('h'),
            atStartOfCurrentLine = isFully(8, 2, 0) && hasCurrentChar('g'),
            atEndOfCurrentLineBeforeNewLine = isFully(10, 2, 2) && hasCurrentChar('i'),
            atEndOfCurrentLineOnNewLine = isFully(11, 2, 3) && isAtEOF,
            atStartOfNextLine = isFully(11, 2, 3) && isAtEOF,
          ),
          relativePositionTest(str1, 11)(
            at = isFully(11, 2, 3) && isAtEOF,
            atStartOfCurrentLine = isFully(8, 2, 0) && hasCurrentChar('g'),
            atEndOfCurrentLineBeforeNewLine = isFully(10, 2, 2) && hasCurrentChar('i'),
            atEndOfCurrentLineOnNewLine = isFully(11, 2, 3) && isAtEOF,
            atStartOfNextLine = isFully(11, 2, 3) && isAtEOF,
          ),
        ),
        suite("str2")(
          relativePositionTest(str2, 3)(
            at = isFully(3, 0, 3) && hasCurrentChar('\n'),
            atStartOfCurrentLine = isFully(0, 0, 0) && hasCurrentChar('a'),
            atEndOfCurrentLineBeforeNewLine = isFully(2, 0, 2) && hasCurrentChar('c'),
            atEndOfCurrentLineOnNewLine = isFully(3, 0, 3) && hasCurrentChar('\n'),
            atStartOfNextLine = isFully(4, 1, 0) && hasCurrentChar('\n'),
          ),
          relativePositionTest(str2, 4)(
            at = isFully(4, 1, 0) && hasCurrentChar('\n'),
            atStartOfCurrentLine = isFully(4, 1, 0) && hasCurrentChar('\n'),
            atEndOfCurrentLineBeforeNewLine = isFully(4, 1, 0) && hasCurrentChar('\n'),
            atEndOfCurrentLineOnNewLine = isFully(4, 1, 0) && hasCurrentChar('\n'),
            atStartOfNextLine = isFully(5, 2, 0) && hasCurrentChar('d'),
          ),
        ),
        suite("str3")(
          relativePositionTest(str3, 0)(
            at = isFully(0, 0, 0) && isAtEOF,
            atStartOfCurrentLine = isFully(0, 0, 0) && isAtEOF,
            atEndOfCurrentLineBeforeNewLine = isFully(0, 0, 0) && isAtEOF,
            atEndOfCurrentLineOnNewLine = isFully(0, 0, 0) && isAtEOF,
            atStartOfNextLine = isFully(0, 0, 0) && isAtEOF,
          ),
        ),
      ),
    )

}
