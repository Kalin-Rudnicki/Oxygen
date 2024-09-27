package oxygen.core.syntax

import oxygen.core.syntax.string.*
import oxygen.test.*
import scala.util.matching.Regex
import zio.test.*

object StringOpsSpec extends OxygenSpecDefault {

  private def splitTest(input: String, regex: Regex)(expStart: Boolean, expEnd: Boolean, expStrs: String*): TestSpec =
    test(s"${input.unesc} : ${regex.regex.unesc}") {
      val (actStart, actStrs, actEnd) = input.detailedSplit(regex)
      assertTrue(
        actStart == expStart,
        actEnd == expEnd,
        actStrs == expStrs,
      )
    }

  override def testSpec: TestSpec =
    suite("StringOpsSpec")(
      suite("empty")(
        splitTest("", "-".r)(false, false, ""),
      ),
      suite("1")(
        splitTest("-|-", "-".r)(true, true, "|"),
        splitTest("|-", "-".r)(false, true, "|"),
        splitTest("-|", "-".r)(true, false, "|"),
        splitTest("|", "-".r)(false, false, "|"),
      ),
      suite("2")(
        splitTest("-[-]-", "-".r)(true, true, "[", "]"),
        splitTest("[-]-", "-".r)(false, true, "[", "]"),
        splitTest("-[-]", "-".r)(true, false, "[", "]"),
        splitTest("[-]", "-".r)(false, false, "[", "]"),
      ),
      suite("4")(
        splitTest("-[-|-|-]-", "-".r)(true, true, "[", "|", "|", "]"),
        splitTest("[-|-|-]-", "-".r)(false, true, "[", "|", "|", "]"),
        splitTest("-[-|-|-]", "-".r)(true, false, "[", "|", "|", "]"),
        splitTest("[-|-|-]", "-".r)(false, false, "[", "|", "|", "]"),
      ),
    )

}
