package oxygen.core

import oxygen.predef.test.*
import scala.util.matching.Regex

object ColorStringSpec extends OxygenSpecDefault {

  private def splitTest(input: ColorString, regex: Regex)(expStart: Boolean, expEnd: Boolean, expStrs: ColorString*): TestSpec =
    test(s"${input.rawString.unesc} : ${regex.regex.unesc}") {
      val (actStart, actStrs, actEnd) = input.detailedSplit(regex)
      assertTrue(actStart == expStart, actEnd == expEnd) &&
      assert(actStrs)(equalTo_unfilteredDiff(expStrs))
    }

  private def interpolationTest(input: ColorString, exp: ColorString): TestSpec =
    test(ColorString.make("[", input, "]").whiteBg.toString) {
      assert(input)(equalTo_unfilteredDiff(exp))
    }

  override def testSpec: TestSpec =
    suite("ColorStringSpec")(
      suite("split")(
        suite("simple")(
          splitTest("", "-".r)(false, false, ""),
          splitTest("[-]", "-".r)(false, false, "[", "]"),
          splitTest("[-|-]", "-".r)(false, false, "[", "|", "]"),
          splitTest("-[-|-]-", "-".r)(true, true, "[", "|", "]"),
        ),
        suite("nested - no color")(
          splitTest(color"-[-${"|"}-]-", "-".r)(true, true, "[", ColorString.make(ColorString.make("|")), "]"),
          splitTest(color"-[${"-|-"}]-", "-".r)(true, true, "[", ColorString.make(ColorString.make("|")), "]"),
          splitTest(color"-[-${"-|-"}-]-", "-".r)(true, true, "[", "", ColorString.make(ColorString.make("|")), "", "]"),
          splitTest(color"[-${"|"}-]", "-".r)(false, false, "[", ColorString.make(ColorString.make("|")), "]"),
          splitTest(color"[${"-|-"}]", "-".r)(false, false, "[", ColorString.make(ColorString.make("|")), "]"),
          splitTest(color"[-${"-|-"}-]", "-".r)(false, false, "[", "", ColorString.make(ColorString.make("|")), "", "]"),
        ),
        suite("nested - color")(
          splitTest(
            color"-[-${color"|-${"...".greenBg}-|".blueFg}-]-".redFg,
            "-".r,
          )(
            true,
            true,
            "[".redFg,
            color"${color"|".blueFg}".redFg,
            color"${color"${color"...".greenBg}".blueFg}".redFg,
            color"${color"|".blueFg}".redFg,
            "]".redFg,
          ),
        ),
      ),
      suite("interpolation")(
        interpolationTest(
          color"",
          ColorString.make(),
        ),
        interpolationTest(
          color"...",
          ColorString.make("..."),
        ),
        interpolationTest(
          color"...".redFg,
          ColorString.make(ColorString.FgBgColor(Color.Named.Red.some, None))("..."),
        ),
        interpolationTest(
          color"...${color"???".blueBg}...".redFg,
          ColorString.make(ColorString.FgBgColor(Color.Named.Red.some, None))(
            "...",
            ColorString.make(ColorString.FgBgColor(None, Color.Named.Blue.some))("???"),
            "...",
          ),
        ),
        interpolationTest(
          color"${color"|".blueFg}".redFg,
          ColorString.make(ColorString.make("|").blueFg).redFg,
        ),
      ),
    )

}
