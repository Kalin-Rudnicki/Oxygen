package oxygen.meta

import oxygen.predef.test.*

object MacroSpec extends OxygenSpecDefault {

  private inline def seqTest[S[_], A](inline values: A*)(exp: S[A]): TestSpec =
    test((exp: Any).toString) {
      val act: S[A] = Macros.make[S, A](values*)
      assertTrue(act == exp)
    }

  override def testSpec: TestSpec =
    suite("MacroSpec")(
      suite("seq")(
        seqTest(1, 2, 3)(List(1, 2, 3)),
        seqTest(1, 2, 3)(Contiguous(1, 2, 3)),
        seqTest(1, 2, 3)(Seq(1, 2, 3)),
        seqTest()(Contiguous()),
        seqTest()(Array[Any]()),
      ),
    )

}
