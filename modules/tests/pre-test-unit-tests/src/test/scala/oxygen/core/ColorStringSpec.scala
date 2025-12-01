package oxygen.core

import oxygen.predef.test.*
import scala.util.matching.Regex

object ColorStringSpec extends OxygenSpecDefault {

  private def splitTest(input: ColorString, regex: Regex)(expStart: Boolean, expEnd: Boolean, expStrs: ColorString*)(using loc: SourceLocation): TestSpec =
    test(s"${input.rawString.unesc} : ${regex.regex.unesc}") {
      val (actStart, actStrs, actEnd) = input.detailedSplit(regex)
      assertTrue(actStart == expStart, actEnd == expEnd) &&
      assert(actStrs)(equalTo_unfilteredDiff(expStrs))
    }

  private def interpolationTest(input: ColorString, exp: ColorString)(using loc: SourceLocation): TestSpec =
    test(ColorString.make("[", input, "]").whiteBg.toString) {
      assert(input)(equalTo_unfilteredDiff(exp))
    }

  private def toStringTest(input: ColorString, colorMode: ColorMode, exp: String)(using loc: SourceLocation): TestSpec =
    test(s"${ColorString.make("[", input, "]").whiteBg} + $colorMode") {
      assertTrue(input.toString(colorMode) == exp)
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
          ColorString.make(ColorState.fg(Color.Named.Red))("..."),
        ),
        interpolationTest(
          color"...${color"???".blueBg}...".redFg,
          ColorString.make(ColorState.fg(Color.Named.Red))(
            "...",
            ColorString.make(ColorState.bg(Color.Named.Blue))("???"),
            "...",
          ),
        ),
        interpolationTest(
          color"${color"|".blueFg}".redFg,
          ColorString.make(ColorString.make("|").blueFg).redFg,
        ),
      ),
      suite("toString")(
        toStringTest(
          color"...${color"???".blueBg}...".redFg,
          ColorMode.Colorless,
          "...???...",
        ),
        toStringTest(
          color"...${color"???".blueBg}...".redFg,
          ColorMode.Extended,
          "\u001b[31m...\u001b[44m???\u001b[49m...\u001b[39m",
        ),
        toStringTest(
          color"...${color"???".blueBg.greenFg}...".redFg,
          ColorMode.Extended,
          "\u001b[31m...\u001b[32;44m???\u001b[31;49m...\u001b[39m",
        ),
        toStringTest(
          ColorString.make("[", color"...${color"???".blueBg.greenFg}...".redFg, "]").magentaBg,
          ColorMode.Extended,
          "\u001b[45m[\u001b[31m...\u001b[32;44m???\u001b[31;45m...\u001b[39m]\u001b[49m",
        ),
        toStringTest(
          color"...${color"???".blueBg}...".redFg,
          ColorMode.Simple,
          "\u001b[31m...\u001b[44m???\u001b[49m...\u001b[39m",
        ),
        toStringTest(
          color"...${color"???".rgbBg(0, 0, 128)}...".redFg,
          ColorMode.Extended,
          "\u001b[31m...\u001b[48;2;0;0;128m???\u001b[49m...\u001b[39m",
        ),
        toStringTest(
          color"...${color"???".rgbBg(0, 0, 128)}...".redFg,
          ColorMode.PreferSimple,
          "\u001b[31m...\u001b[48;2;0;0;128m???\u001b[49m...\u001b[39m",
        ),
        toStringTest(
          color"...${color"???".rgbBg(0, 0, 128)}...".redFg,
          ColorMode.Simple,
          "\u001b[31m...???...\u001b[39m",
        ),
        toStringTest(
          color"...${color"???".rgbBg(0, 0, 128)}...".redFg,
          ColorMode.ShowColorName,
          "[[fg:red]]...[[bg:rgb(0, 0, 128)]]???[[bg:default]]...[[fg:default]]",
        ),
      ),
    )

}
