package oxygen.slyce.core.generic

import oxygen.predef.test.*

object ParsedRegexSpec extends OxygenSpecDefault {

  private def parseSpec(in: String)(exp: ParsedRegex)(using Trace, SourceLocation): TestSpec =
    test(in) {
      assert(ParsedRegex.parse(in))(isRight(equalTo(exp)))
    }

  override def testSpec: TestSpec =
    suite("ParsedRegexSpec")(
      suite("parse")(
        parseSpec("")(ParsedRegex.Sequence.ofElems()),
        parseSpec("abc")(ParsedRegex.Sequence.ofElems('a', 'b', 'c')),
        parseSpec("abc")(ParsedRegex.Sequence.ofString("abc")),
        parseSpec("abc")("abc"),
        parseSpec("abc|def")(ParsedRegex.Group.of(ParsedRegex.Sequence.ofString("abc"), ParsedRegex.Sequence.ofString("def"))),
        parseSpec("abc|def")(ParsedRegex.Sequence.ofString("abc") | ParsedRegex.Sequence.ofString("def")),
        parseSpec("abc|def|ghi")(ParsedRegex.Group.of("abc", "def", "ghi")),
        parseSpec("(abc|def)(ghi|jkl)")(ParsedRegex.Sequence.ofElems(ParsedRegex.Group.of("abc", "def"), ParsedRegex.Group.of("ghi", "jkl"))),
        parseSpec("(abc|def)|(ghi|jkl)")(ParsedRegex.Group.of("abc", "def", "ghi", "jkl")),
        parseSpec("(abc|def)?(ghi|jkl)+")(ParsedRegex.Sequence.ofElems(ParsedRegex.Group.of("abc", "def").?, ParsedRegex.Group.of("ghi", "jkl").+)),
        parseSpec(".")(ParsedRegex.CharClass.anything),
        parseSpec("a|b")(ParsedRegex.CharClass.inclusive('a', 'b')),
        parseSpec("a|bc")(ParsedRegex.Sequence.ofString("a") | ParsedRegex.Sequence.ofString("bc")),
        parseSpec("ab|c")(ParsedRegex.Sequence.ofString("ab") | ParsedRegex.Sequence.ofString("c")),
        parseSpec("(abc)?")(ParsedRegex.Sequence.ofString("abc").?),
        parseSpec("(abc)*")(ParsedRegex.Sequence.ofString("abc").*),
        parseSpec("(abc)+")(ParsedRegex.Sequence.ofString("abc").+),
        parseSpec("(abc){3}")(ParsedRegex.Sequence.ofString("abc").exactly(3)),
        parseSpec("(abc){3,}")(ParsedRegex.Sequence.ofString("abc").atLeast(3)),
        parseSpec("(abc){,3}")(ParsedRegex.Sequence.ofString("abc").atMost(3)),
        parseSpec("(abc){1,3}")(ParsedRegex.Sequence.ofString("abc").between(1, 3)),
      ),
    )

}
