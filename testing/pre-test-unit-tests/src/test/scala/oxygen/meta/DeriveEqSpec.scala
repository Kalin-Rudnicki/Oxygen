package oxygen.meta

import oxygen.meta.example.*
import oxygen.predef.test.*

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

  given Eq[CaseClass1 | CaseClass2 | CaseClass4] = Eq.derivedUnion

  given Eq[ALike] = (a, b) => a.a == b.a
  given Eq[BLike] = (a, b) => a.b == b.b
  given Eq[CLike] = (a, b) => a.c == b.c

  type ABLike = ALike & BLike
  type ABCLike = ALike & BLike & CLike

  given Eq[ABLike] = Eq.derivedIntersection
  given Eq[ABCLike] = Eq.derivedIntersection

  final case class ABC(a: Int, b: Int, c: Int) extends ALike, BLike, CLike

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

  private def unionSpec: TestSpec =
    suite("union")(
      suite("CaseClass1 | CaseClass2 | CaseClass4")(
        makePassingTest[CaseClass1 | CaseClass2 | CaseClass4]("1 - 1 : eq")(
          CaseClass1(0, "", false),
          CaseClass1(0, "", false),
        ),
        makePassingTest[CaseClass1 | CaseClass2 | CaseClass4]("2 - 2 : eq")(
          CaseClass2(),
          CaseClass2(),
        ),
        makeFailingTest[CaseClass1 | CaseClass2 | CaseClass4]("1 - 1 : not-eq")(
          CaseClass1(0, "", false),
          CaseClass1(1, "_", true),
        ),
        makeFailingTest[CaseClass1 | CaseClass2 | CaseClass4]("1 - 2 : not-eq")(
          CaseClass1(0, "", false),
          CaseClass2(),
        ),
        makePassingTest[CaseClass1 | CaseClass2 | CaseClass4]("4 - 4 : eq")(
          CaseClass4(1, None),
          CaseClass4(1, None),
        ),
        makeFailingTest[CaseClass1 | CaseClass2 | CaseClass4]("4 - 4 : not-eq")(
          CaseClass4(1, None),
          CaseClass4(2, None),
        ),
        makeFailingTest[CaseClass1 | CaseClass2 | CaseClass4]("1 - 4 : not-eq")(
          CaseClass1(0, "", false),
          CaseClass4(2, None),
        ),
      ),
    )

  private def intersectionSpec: TestSpec =
    suite("IntersectionSpec")(
      suite("ALike & BLike & CLike")(
        makePassingTest[ABCLike]("all same - eq")(
          ABC(0, 0, 0),
          ABC(0, 0, 0),
        ),
        makeFailingTest[ABCLike]("a different - not-eq")(
          ABC(0, 0, 0),
          ABC(1, 0, 0),
        ),
        makeFailingTest[ABCLike]("c different - not-eq")(
          ABC(0, 0, 0),
          ABC(0, 0, 1),
        ),
      ),
      suite("ALike & BLike")(
        makePassingTest[ABLike]("all same - eq")(
          ABC(0, 0, 0),
          ABC(0, 0, 0),
        ),
        makeFailingTest[ABLike]("a different - not-eq")(
          ABC(0, 0, 0),
          ABC(1, 0, 0),
        ),
        makePassingTest[ABLike]("c different - eq")(
          ABC(0, 0, 0),
          ABC(0, 0, 1),
        ),
      ),
    )

  override def testSpec: TestSpec =
    suite("DeriveEqSpec")(
      productSpec,
      sumSpec,
      unionSpec,
      intersectionSpec,
    )

}
