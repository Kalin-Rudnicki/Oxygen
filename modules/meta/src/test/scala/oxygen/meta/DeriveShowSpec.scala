package oxygen.meta

import oxygen.meta.example.*
import oxygen.predef.test.*
import scala.quoted.*

object DeriveShowSpec extends OxygenSpecDefault {

  private def makeTest[A: {Show as show}](label: String)(value: A, exp: String): TestSpec =
    test(label) {
      assertTrue(
        show.show(value) == exp,
      )
    }

  given Show[CaseClass1] = Show.derived
  given Show[CaseClass2] = Show.derived
  given Show[CaseClass4] = Show.derived
  given Show[CaseObject1.type] = Show.derived
  given Show[Enum1.Case1] = Show.derived
  given Show[Enum1] = Show.derived

  private def productSpec: TestSpec =
    suite("product")(
      suite("CaseClass1")(
        makeTest("false")(
          CaseClass1(0, "no", false),
          // TODO (KR) : swap these when field annotations start working
          // """{ my-int = 0, string = "no", boolean = false }""",
          """{ int = 0, string = "no", boolean = false }""",
        ),
        makeTest("true")(
          CaseClass1(1, "yes", true),
          """{ int = 1, string = "yes", boolean = true }""",
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
    )

  override def testSpec: TestSpec =
    suite("DeriveShowSpec")(
      productSpec,
      sumSpec,
    )

}
