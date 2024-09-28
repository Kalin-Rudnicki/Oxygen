package oxygen.core

import oxygen.predef.test.*

object IndentedStringSpec extends OxygenSpecDefault {

  private def makeTest(label: String)(input: IndentedString, exp: String): TestSpec =
    test(label) {
      assertTrue(input.toString("| ") == exp)
    }

  override def testSpec: TestSpec =
    suite("IndentedStringSpec")(
      makeTest("break")(IndentedString.Break, ""),
      makeTest("str")(IndentedString.Str("A"), "A"),
      makeTest("strs")(IndentedString.inline("A", "B", "C"), "A\nB\nC"),
      makeTest("section")(IndentedString.section("section")("A", "B", "C"), "section\n| A\n| B\n| C"),
      makeTest("prefix-str")(IndentedString.Str("A").prefix("p:", " "), "p: A"),
      makeTest("prefix-strs")(
        IndentedString
          .inline(
            IndentedString.inline(
              "A",
              "B",
            ),
            IndentedString.indented(
              "C",
              "D",
            ),
          )
          .prefix("p:", " "),
        "p: A\nB\n| C\n| D",
      ),
      makeTest("prefix-indented")(
        IndentedString
          .indented(
            "A",
            "B",
            "C",
            "D",
          )
          .prefix("p:", " "),
        "p:\n| A\n| B\n| C\n| D",
      ),
      makeTest("sections")(
        IndentedString.section("1:")(
          IndentedString.section("2:")("A", "B"),
          IndentedString.section("3:")("C", "D"),
        ),
        "1:\n| 2:\n| | A\n| | B\n| 3:\n| | C\n| | D",
      ),
    )

}
