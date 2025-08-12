package oxygen.test

import oxygen.predef.test.*

object TestExamplesSpec extends OxygenSpecDefault {

  val gen1: Generator[Int] = Generator.finite(1, 2, 3)
  val gen2: Generator[Int] = Generator.Rand(Random.nextIntBetween(10, 20))
  val gen3: Generator[(MyEnum1, MyEnum1)] =
    for {
      a <- Generator[MyEnum1]
      b <- Generator[MyEnum1]
    } yield (a, b)

  enum MyEnum1 derives Generator { case A, B, C }

  enum MyEnum2 derives Generator {
    case A(bool: Boolean, enum1: MyEnum1)
    case B
  }

  override def testSpec: TestSpec =
    suite("TestExamplesSpec")(
      test("finite") {
        for {
          a <- gen1.genExhaustiveOrSized
          b <- gen1.genN(10)
          c <- Generator.option(using gen1).genExhaustiveOrSized
        } yield assertTrue(
          a.toSet == Set(1, 2, 3),
          b.length == 10,
          (b.toSet &~ Set(1, 2, 3)).isEmpty,
          c.toSet == Set(1.some, 2.some, 3.some, None),
        )
      },
      test("flatMap") {
        for {
          a <- gen3.genExhaustiveOrSized
        } yield assertTrue(
          a.toSet == Set(
            (MyEnum1.A, MyEnum1.A),
            (MyEnum1.A, MyEnum1.B),
            (MyEnum1.A, MyEnum1.C),
            (MyEnum1.B, MyEnum1.A),
            (MyEnum1.B, MyEnum1.B),
            (MyEnum1.B, MyEnum1.C),
            (MyEnum1.C, MyEnum1.A),
            (MyEnum1.C, MyEnum1.B),
            (MyEnum1.C, MyEnum1.C),
          ),
        )
      },
      test("gen") {
        for {
          a <- gen2.genExhaustiveOrSized
          b <- gen2.genExhaustiveOrSizedWithSize(5)
        } yield assertTrue(
          a.forall(a => a >= 10 && a < 20),
          b.forall(a => a >= 10 && a < 20),
          b.length == 5,
        )
      },
      test("derived") {
        for {
          a <- Generator[MyEnum2].genExhaustiveOrSized
        } yield assertTrue(
          a.toSet ==
            Set(
              MyEnum2.A(true, MyEnum1.A),
              MyEnum2.A(true, MyEnum1.B),
              MyEnum2.A(true, MyEnum1.C),
              MyEnum2.A(false, MyEnum1.A),
              MyEnum2.A(false, MyEnum1.B),
              MyEnum2.A(false, MyEnum1.C),
              MyEnum2.B,
            ),
        )
      },
    )

}
