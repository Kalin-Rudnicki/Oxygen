package oxygen.meta

import oxygen.meta.example.*
import oxygen.predef.test.*

object DeriveShowSpec extends OxygenSpecDefault {

  private def makeTest[A: {Show as show}](label: String)(value: A, exp: String): TestSpec =
    test(label) {
      assertTrue(
        show.show(value) == exp,
      )
    }

  // given [A: Show] => Show[Option[A]] = Show.derived

  given [A: {Show as show}] => Show[Option[A]] = {
    case Some(value) => show.show(value)
    case None        => "none"
  }

  given Show[CaseClass1] = Show.derived
  given Show[CaseClass2] = Show.derived
  given Show[CaseClass4] = Show.derived
  given [A: Show] => Show[CaseClass6[A]] = Show.derived

  given Show[CaseObject1.type] = Show.derived
  given Show[Enum1.Case1] = Show.derived
  given Show[Enum1] = Show.derived
  given [A: Show] => Show[SealedTrait3.A[A]] = Show.derived

  private def productSpec: TestSpec =
    suite("product")(
      suite("CaseClass1")(
        makeTest("false")(
          CaseClass1(0, "no", false),
          """{ my-int = 0, string = "no", boolean = false }""",
        ),
        makeTest("true")(
          CaseClass1(1, "yes", true),
          """{ my-int = 1, string = "yes", boolean = true }""",
        ),
      ),
      makeTest("CaseClass2")(
        CaseClass2(),
        """{}""",
      ),
      suite("CaseClass4")(
        makeTest("no nesting")(
          CaseClass4(0, None),
          """{ value = 0, nested = none }""",
        ),
        makeTest("nesting")(
          CaseClass4(1, CaseClass4(0, None).some),
          """{ value = 1, nested = { value = 0, nested = none } }""",
        ),
      ),
      makeTest("CaseObject1")(
        CaseObject1,
        """{}""",
      ),
      suite("CaseClass6")(
        makeTest("int")(
          CaseClass6(5),
          """{ field = 5 }""",
        ),
        makeTest("CaseClass1")(
          CaseClass6(CaseClass1(0, "no", false)),
          """{ field = { my-int = 0, string = "no", boolean = false } }""",
        ),
      ),
      makeTest("SealedTrait3.A")(
        SealedTrait3.A(1),
        """{ a = 1 }""",
      ),
    )

  private def sumSpec: TestSpec =
    suite("sum")(
      suite("Enum1")(
        makeTest[Enum1]("Case1")(
          Enum1.Case1(0, "0"),
          """Case1: { int = 0, string = "0" }""",
        ),
        makeTest[Enum1]("Case2")(
          Enum1.Case2(),
          """Case2: {}""",
        ),
        makeTest[Enum1]("Case3")(
          Enum1.Case3,
          """Case3: {}""",
        ),
      ),
      suite("SealedTrait3")(
        makeTest[SealedTrait3[Int, String]]("A")(
          SealedTrait3.A(1),
          """A: { a = 1 }""",
        ),
        makeTest[SealedTrait3[Int, String]]("B")(
          SealedTrait3.B("s"),
          """B: { b = "s" }""",
        ),
        makeTest[SealedTrait3[Int, String]]("AB")(
          SealedTrait3.AB1(1, "s"),
          """AB1: { a = 1, b = "s" }""",
        ),
        makeTest[SealedTrait3[Int, String]]("AB")(
          SealedTrait3.AB2("s", 1),
          """AB2: { a = "s", b = 1 }""",
        ),
        makeTest[SealedTrait3[Int, String]]("Neither")(
          SealedTrait3.Neither,
          """Neither: {}""",
        ),
      ),
      suite("Enum2")(
        makeTest[Enum2[Int, String]]("A")(
          Enum2.A(1),
          """A: { a = 1 }""",
        ),
        makeTest[Enum2[Int, String]]("B")(
          Enum2.B("s"),
          """B: { b = "s" }""",
        ),
        makeTest[Enum2[Int, String]]("AB")(
          Enum2.AB1(1, "s"),
          """AB1: { a = 1, b = "s" }""",
        ),
        makeTest[Enum2[Int, String]]("AB")(
          Enum2.AB2("s", 1),
          """AB2: { a = "s", b = 1 }""",
        ),
        makeTest[Enum2[Int, String]]("Neither")(
          Enum2.Neither,
          """Neither: {}""",
        ),
      ),
    )

  override def testSpec: TestSpec =
    suite("DeriveShowSpec")(
      productSpec,
      sumSpec,
    )

}
