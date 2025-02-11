package oxygen.meta

import oxygen.meta.example.*
import oxygen.predef.test.*
import scala.quoted.*

object DeriveEqSpec extends OxygenSpecDefault {

  private def makePassingTest[A: {Eq as eq}](label: String)(a: A): TestSpec =
    makePassingTest(label)(a, a)

  private def makePassingTest[A: {Eq as eq}](label: String)(a: A, b: A): TestSpec =
    test(label) {
      assertTrue(
        eq.areEqual(a, b),
      )
    }

  private def makeFailingTest[A: {Eq as eq}](label: String)(a: A, b: A): TestSpec =
    test(label) {
      assertTrue(
        !eq.areEqual(a, b),
      )
    }

  given Eq[CaseClass1] = Eq.derived
  given Eq[CaseClass2] = Eq.derived
  given Eq[CaseClass4] = Eq.derived
  given Eq[CaseObject1.type] = Eq.derived
  given Eq[Enum1] = Eq.derived

  private def productSpec: TestSpec =
    suite("product")(
      suite("CaseClass1")(
        makePassingTest("eq")(
          CaseClass1(1, "yes", true),
        ),
        makeFailingTest("not-eq-1")(
          CaseClass1(0, "yes", true),
          CaseClass1(1, "yes", true),
        ),
        makeFailingTest("not-eq-2")(
          CaseClass1(1, "no", true),
          CaseClass1(1, "yes", true),
        ),
        makeFailingTest("not-eq-3")(
          CaseClass1(1, "yes", false),
          CaseClass1(1, "yes", true),
        ),
      ),
      makePassingTest("CaseClass2")(
        CaseClass2(),
      ),
      suite("CaseClass4")(
        makePassingTest("no nesting")(
          CaseClass4(0, None),
        ),
        makePassingTest("nesting")(
          CaseClass4(1, CaseClass4(0, None).some),
        ),
      ),
      makePassingTest[CaseObject1.type]("CaseObject1")(
        CaseObject1,
      ),
    )

  private def sumSpec: TestSpec =
    suite("sum")(
      suite("Enum1")(
        makePassingTest[Enum1]("Case1 - eq")(
          Enum1.Case1(0, "0"),
        ),
        makeFailingTest[Enum1]("Case1 - not-eq")(
          Enum1.Case1(0, "0"),
          Enum1.Case1(0, "1"),
        ),
        makePassingTest[Enum1]("Case2")(
          Enum1.Case2(),
        ),
        makePassingTest[Enum1]("Case3")(
          Enum1.Case3,
        ),
        makeFailingTest[Enum1]("Case1/Case2")(
          Enum1.Case1(0, "0"),
          Enum1.Case2(),
        ),
        makeFailingTest[Enum1]("Case1/Case3")(
          Enum1.Case1(0, "0"),
          Enum1.Case3,
        ),
        makeFailingTest[Enum1]("Case2/Case3")(
          Enum1.Case2(),
          Enum1.Case3,
        ),
      ),
    )

  override def testSpec: TestSpec =
    suite("DeriveEqSpec")(
      productSpec,
      sumSpec,
    )

}
