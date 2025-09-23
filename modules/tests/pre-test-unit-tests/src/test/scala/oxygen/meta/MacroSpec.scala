package oxygen.meta

import oxygen.predef.test.*

object MacroSpec extends OxygenSpecDefault {

  private inline def seqTest[S[_], A](inline values: A*)(exp: S[A]): TestSpec =
    test((exp: Any).toString) {
      val act: S[A] = Macros.seqFun[S, A](values*)
      assertTrue(act == exp)
    }

  final case class Class1(a: Int = 5, b: Option[String] = None, c: Boolean)

  sealed trait Sum1
  object Sum1 {
    case object Case1 extends Sum1
    case object Case2 extends Sum1
    case object Case3 extends Sum1
  }

  sealed trait Sum2
  object Sum2 {
    case object Case1 extends Sum2
    case object Case2 extends Sum2
    case object Case3 extends Sum2
    final case class Other(s: String) extends Sum2
  }

  type Sum3 = Sum1.Case1.type | Sum2.Case1.type

  type Sum4 = Sum1 | Sum2

  type Sum5 = Sum1.Case1.type

  object ProductTransforms {

    case object CaseObject0

    final case class CaseClass0()
    final case class CaseClass1(i: Int)
    final case class CaseClass2(i: Int, s: String, b: Boolean)

  }

  private inline def transformTest[Source, Target](source: Source, target: Target): TestSpec =
    test(s"$source <-> $target") {
      val (ab, ba) = K0.ProductGeneric.deriveTransform[Source, Target]
      assertTrue(
        ab(source) == target,
        ba(target) == source,
      )
    }

  override def testSpec: TestSpec =
    suite("MacroSpec")(
      suite("seq")(
        seqTest(1, 2, 3)(List(1, 2, 3)),
        seqTest(1, 2, 3)(Seq(1, 2, 3)),
        seqTest()(Seq()),
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
      suite("enum values")(
        test("Sum1") {
          assertTrue(
            K0.SumGeneric.EnumGeneric.deriveEnum.strictEnum.values[Sum1].toSet ==
              Set(Sum1.Case1, Sum1.Case2, Sum1.Case3),
          )
        },
        test("Sum2") {
          assertTrue(
            K0.SumGeneric.EnumGeneric.deriveEnum.ignoreSingleCaseClass.values[Sum2].toSet ==
              Set(Sum2.Case1, Sum2.Case2, Sum2.Case3),
          )
        },
        test("Sum3") {
          assertTrue(
            K0.SumGeneric.EnumGeneric.deriveEnum.strictEnum.values[Sum3].toSet ==
              Set(Sum1.Case1, Sum2.Case1),
          )
        },
        test("Sum4") {
          assertTrue(
            K0.SumGeneric.EnumGeneric.deriveEnum.ignoreSingleCaseClass.values[Sum4].toSet ==
              Set(Sum1.Case1, Sum1.Case2, Sum1.Case3, Sum2.Case1, Sum2.Case2, Sum2.Case3),
          )
        },
        test("Sum5") {
          assertTrue(
            K0.SumGeneric.EnumGeneric.deriveEnum.strictEnum.values[Sum5].toSet ==
              Set(Sum1.Case1),
          )
        },
      ),
      suite("product transform")(
        transformTest((), ProductTransforms.CaseObject0),
        transformTest((), ProductTransforms.CaseClass0()),
        transformTest(5, ProductTransforms.CaseClass1(5)),
        transformTest((5, "s", true), ProductTransforms.CaseClass2(5, "s", true)),
      ),
    )

}
