package oxygen.slyce.core.generic

import oxygen.predef.test.*
import oxygen.slyce.core.generic.ParsedRegex.*

object ParsedRegexSpec extends OxygenSpecDefault {

  private def parseSpec(in: String)(exp: ParsedRegex)(using Trace, SourceLocation): TestSpec =
    test(in) {
      assert(parse(in))(isRight(equalTo_filteredDiff(exp)))
    }

  override def testSpec: TestSpec =
    suite("ParsedRegexSpec")(
      suite("parse")(
        parseSpec("")(Sequence.ofElems()),
        parseSpec("abc")(Sequence.ofElems('a', 'b', 'c')),
        parseSpec("abc")(Sequence.ofString("abc")),
        parseSpec("abc")("abc"),
        parseSpec("abc|def")(Group.of(Sequence.ofString("abc"), Sequence.ofString("def"))),
        parseSpec("abc|def")(Sequence.ofString("abc") | Sequence.ofString("def")),
        parseSpec("abc|def|ghi")(Group.of("abc", "def", "ghi")),
        parseSpec("(abc|def)(ghi|jkl)")(Sequence.ofElems(Group.of("abc", "def"), Group.of("ghi", "jkl"))),
        parseSpec("(abc|def)|(ghi|jkl)")(Group.of("abc", "def", "ghi", "jkl")),
        parseSpec("(abc|def)?(ghi|jkl)+")(Sequence.ofElems(Group.of("abc", "def").?, Group.of("ghi", "jkl").+)),
        parseSpec(".")(CharClass.anything),
        parseSpec("a|b")(CharClass.inclusive('a', 'b')),
        parseSpec("a|bc")(Sequence.ofString("a") | Sequence.ofString("bc")),
        parseSpec("ab|c")(Sequence.ofString("ab") | Sequence.ofString("c")),
        parseSpec("(abc)?")(Sequence.ofString("abc").?),
        parseSpec("(abc)*")(Sequence.ofString("abc").*),
        parseSpec("(abc)+")(Sequence.ofString("abc").+),
        parseSpec("(abc){3}")(Sequence.ofString("abc").exactly(3)),
        parseSpec("(abc){3,}")(Sequence.ofString("abc").atLeast(3)),
        parseSpec("(abc){,3}")(Sequence.ofString("abc").atMost(3)),
        parseSpec("(abc){1,3}")(Sequence.ofString("abc").between(1, 3)),
        parseSpec("abc(de(f|g)hi)?jkl")(Sequence.ofElems("abc", Sequence.ofElems("de", CharClass.inclusive('f', 'g'), "hi").?, "jkl")),
      ),
    )

}
