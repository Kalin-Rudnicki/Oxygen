package oxygen.core

import oxygen.predef.test.*

object TextSpec extends OxygenSpecDefault {

  private def makeTest(exp: String, cfg: => Text.Config)(in: Text)(using Trace, SourceLocation): TestSpec =
    test(exp.unesc) {
      simpleEqual(in.lazyStringBuildRootSimple(cfg), exp)
    }

  private val showColor: Text.Config = Text.Config.make(ColorMode.ShowColorName, ">> ")

  override def testSpec: TestSpec =
    suite("TextSpec")(
      makeTest("abc", showColor) { Text.fromString("abc") },
      makeTest("abc+def", showColor) { Text.fromString("abc") ++ Text.fromString("+") ++ Text.fromString("def") },
      makeTest("abc\ndef", showColor) { Text.fromString("abc") ++ Text.newLine ++ Text.fromString("def") },
      makeTest("abc+", showColor) { Text.fromString("abc") ++ Text.fromString("+") ++ Text.fromString("def").when(false).indented(">> ") },
      makeTest("abc+\n>> def", showColor) { Text.fromString("abc") ++ Text.fromString("+") ++ Text.fromString("def").indented(">> ") },
      makeTest("[[fg:red]]abc[[fg:default]]", showColor) { Text.fromString("abc").colorizeFg(Color.Named.Red) },
      makeTest("[[fg:red]]abc[[bg:blue]]def[[bg:default]]ghi[[fg:default]]", showColor) {
        (
          Text.fromString("abc") ++
            Text.fromString("def").colorizeBg(Color.Named.Blue) ++
            Text.fromString("ghi")
        ).colorizeFg(Color.Named.Red)
      },
      makeTest("[[fg:red]]abc[[fg:default]]\n[[fg:red]]def[[fg:default]]", showColor)(Text.fromString("abc\ndef").colorizeFg(Color.Named.Red)),
      makeTest("(A)\n>> (B)\n>> >> (C)", showColor) {
        Text.fromString("(A)") |> { Text.fromString("(B)") |> Text.fromString("(C)") }
      },
      makeTest("(A)", showColor) {
        Text.fromString("(A)") |> { Text.empty }
      },
      makeTest("abc\n defghi\nghi1", showColor) {
        str"""abc
             | def${"ghi"}
             |ghi1"""
      },
      makeTest("abc\n defghi\nghi2", showColor) {
        str"""abc
             | defghi
             |ghi2"""
      },
      makeTest("abc+defghi+ghi3", showColor) {
        str"""abc+def${"ghi"}+ghi${3.toString}"""
      },
      makeTest("abc+defghi+ghi4", showColor) {
        str"""abc+defghi+ghi4"""
      },
      makeTest("abc\n def\nghi\nghi5", showColor) {
        str"""abc
             | def\nghi
             |ghi5"""
      },
    )

}
