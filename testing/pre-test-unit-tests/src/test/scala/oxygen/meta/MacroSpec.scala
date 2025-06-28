package oxygen.meta

import oxygen.predef.test.*

object MacroSpec extends OxygenSpecDefault {

  private inline def seqTest[S[_], A](inline values: A*)(exp: S[A]): TestSpec =
    test((exp: Any).toString) {
      val act: S[A] = Macros.seqFun[S, A](values*)
      assertTrue(act == exp)
    }

  final case class Class1(a: Int = 5, b: Option[String] = None, c: Boolean)

  override def testSpec: TestSpec =
    suite("MacroSpec")(
      suite("seq")(
        seqTest(1, 2, 3)(List(1, 2, 3)),
        seqTest(1, 2, 3)(Contiguous(1, 2, 3)),
        seqTest(1, 2, 3)(Seq(1, 2, 3)),
        seqTest()(Contiguous()),
      ),
      suite("matching")(
        suite("strings")(
          test("const + const") {
            assertTrue(Macros.stringMatch("const", "const") == "<const>")
          },
          test("abc + const") {
            assertTrue(Macros.stringMatch("abc", "const") == "abc")
          },
          test("const + def") {
            assertTrue(Macros.stringMatch("const", "def") == "def")
          },
          test("abc + def") {
            assertTrue(Macros.stringMatch("abc", "def") == "abc + def")
          },
        ),
      ),
      suite("default args")(
        test("Class1") {
          assertTrue(Macros.defaultArgs[Class1] == "[a = 5, b = None]")
        },
      ),
    )

}
