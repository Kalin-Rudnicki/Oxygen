package oxygen.core

import oxygen.predef.test.*

object LazyStringSpec extends OxygenSpecDefault {

  private def makeTest(exp: String, cfg: => LazyString.Config)(in: LazyString)(using Trace, SourceLocation): TestSpec =
    test(exp.unesc) {
      simpleEqual(in.buildNowSimple(cfg), exp)
    }

  private val showColor: LazyString.Config = LazyString.Config.make(ColorMode.ShowColorName, ">> ")

  override def testSpec: TestSpec =
    suite("LazyStringSpec")(
      makeTest("abc", showColor) { LazyString.fromString("abc") },
      makeTest("abc+def", showColor) { LazyString.fromString("abc") ++ LazyString.fromString("+") ++ LazyString.fromString("def") },
      makeTest("abc\ndef", showColor) { LazyString.fromString("abc") ++ LazyString.newLine ++ LazyString.fromString("def") },
      makeTest("abc+def", showColor) { LazyString.fromString("abc") ++ LazyString.fromString("+") ++ LazyString.fromString("def").indented(">> ") },
      makeTest("abc+\n>> def", showColor) { LazyString.fromString("abc") ++ LazyString.fromString("+") ++ LazyString.fromString("\ndef").indented(">> ") },
      makeTest("[[fg:red]]abc[[fg:default]]", showColor) { LazyString.fromString("abc").colorizeFg(Color.Named.Red) },
      makeTest("[[fg:red]]abc[[bg:blue]]def[[bg:default]]ghi[[fg:default]]", showColor) {
        (
          LazyString.fromString("abc") ++
            LazyString.fromString("def").colorizeBg(Color.Named.Blue) ++
            LazyString.fromString("ghi")
        ).colorizeFg(Color.Named.Red)
      },
      makeTest("[[fg:red]]abc[[fg:default]]\n[[fg:red]]def[[fg:default]]", showColor)(LazyString.fromString("abc\ndef").colorizeFg(Color.Named.Red)),
      makeTest("(A)\n>> (B)\n>> >> (C)", showColor) {
        LazyString.fromString("(A)") |> { LazyString.fromString("(B)") |> LazyString.fromString("(C)") }
      },
      makeTest("(A)", showColor) {
        LazyString.fromString("(A)") |> { LazyString.empty }
      },
    )

}
