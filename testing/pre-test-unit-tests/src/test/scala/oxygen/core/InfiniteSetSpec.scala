package oxygen.core

import oxygen.predef.test.*

object InfiniteSetSpec extends OxygenSpecDefault {

  enum MyEnum { case A, B, C, D, E }
  import InfiniteSet.{Exclusive, Inclusive}
  import MyEnum.{A, B, C, D}

  val inclusive1: InfiniteSet[MyEnum] = Inclusive(A, B)
  val inclusive2: InfiniteSet[MyEnum] = Inclusive(B, C)
  val inclusive3: InfiniteSet[MyEnum] = Inclusive(C, D)

  val exclusive1: InfiniteSet[MyEnum] = Exclusive(A, B)
  val exclusive2: InfiniteSet[MyEnum] = Exclusive(B, C)
  val exclusive3: InfiniteSet[MyEnum] = Exclusive(C, D)

  private def binOpSuite(label: String)(op: (InfiniteSet[MyEnum], InfiniteSet[MyEnum]) => InfiniteSet[MyEnum])(
      // (inclusive1, _)
      `(inclusive1, inclusive1)`: => InfiniteSet[MyEnum],
      `(inclusive1, inclusive2)`: => InfiniteSet[MyEnum],
      `(inclusive1, inclusive3)`: => InfiniteSet[MyEnum],
      `(inclusive1, exclusive1)`: => InfiniteSet[MyEnum],
      `(inclusive1, exclusive2)`: => InfiniteSet[MyEnum],
      `(inclusive1, exclusive3)`: => InfiniteSet[MyEnum],
      // (inclusive2, _)
      `(inclusive2, inclusive1)`: => InfiniteSet[MyEnum],
      `(inclusive2, inclusive2)`: => InfiniteSet[MyEnum],
      `(inclusive2, inclusive3)`: => InfiniteSet[MyEnum],
      `(inclusive2, exclusive1)`: => InfiniteSet[MyEnum],
      `(inclusive2, exclusive2)`: => InfiniteSet[MyEnum],
      `(inclusive2, exclusive3)`: => InfiniteSet[MyEnum],
      // (inclusive3, _)
      `(inclusive3, inclusive1)`: => InfiniteSet[MyEnum],
      `(inclusive3, inclusive2)`: => InfiniteSet[MyEnum],
      `(inclusive3, inclusive3)`: => InfiniteSet[MyEnum],
      `(inclusive3, exclusive1)`: => InfiniteSet[MyEnum],
      `(inclusive3, exclusive2)`: => InfiniteSet[MyEnum],
      `(inclusive3, exclusive3)`: => InfiniteSet[MyEnum],
      // (exclusive1, _)
      `(exclusive1, inclusive1)`: => InfiniteSet[MyEnum],
      `(exclusive1, inclusive2)`: => InfiniteSet[MyEnum],
      `(exclusive1, inclusive3)`: => InfiniteSet[MyEnum],
      `(exclusive1, exclusive1)`: => InfiniteSet[MyEnum],
      `(exclusive1, exclusive2)`: => InfiniteSet[MyEnum],
      `(exclusive1, exclusive3)`: => InfiniteSet[MyEnum],
      // (exclusive2, _)
      `(exclusive2, inclusive1)`: => InfiniteSet[MyEnum],
      `(exclusive2, inclusive2)`: => InfiniteSet[MyEnum],
      `(exclusive2, inclusive3)`: => InfiniteSet[MyEnum],
      `(exclusive2, exclusive1)`: => InfiniteSet[MyEnum],
      `(exclusive2, exclusive2)`: => InfiniteSet[MyEnum],
      `(exclusive2, exclusive3)`: => InfiniteSet[MyEnum],
      // (exclusive3, _)
      `(exclusive3, inclusive1)`: => InfiniteSet[MyEnum],
      `(exclusive3, inclusive2)`: => InfiniteSet[MyEnum],
      `(exclusive3, inclusive3)`: => InfiniteSet[MyEnum],
      `(exclusive3, exclusive1)`: => InfiniteSet[MyEnum],
      `(exclusive3, exclusive2)`: => InfiniteSet[MyEnum],
      `(exclusive3, exclusive3)`: => InfiniteSet[MyEnum],
  ): TestSpec = {
    def makeTest(set1: InfiniteSet[MyEnum], set2: InfiniteSet[MyEnum])(exp: => InfiniteSet[MyEnum]): TestSpec =
      test((set1, set2).toString) {
        assertTrue(
          op(set1, set2) == exp,
        )
      }

    suite(label)(
      // (inclusive1, _)
      makeTest(inclusive1, inclusive1)(`(inclusive1, inclusive1)`),
      makeTest(inclusive1, inclusive2)(`(inclusive1, inclusive2)`),
      makeTest(inclusive1, inclusive3)(`(inclusive1, inclusive3)`),
      makeTest(inclusive1, exclusive1)(`(inclusive1, exclusive1)`),
      makeTest(inclusive1, exclusive2)(`(inclusive1, exclusive2)`),
      makeTest(inclusive1, exclusive3)(`(inclusive1, exclusive3)`),
      // (inclusive2, _)
      makeTest(inclusive2, inclusive1)(`(inclusive2, inclusive1)`),
      makeTest(inclusive2, inclusive2)(`(inclusive2, inclusive2)`),
      makeTest(inclusive2, inclusive3)(`(inclusive2, inclusive3)`),
      makeTest(inclusive2, exclusive1)(`(inclusive2, exclusive1)`),
      makeTest(inclusive2, exclusive2)(`(inclusive2, exclusive2)`),
      makeTest(inclusive2, exclusive3)(`(inclusive2, exclusive3)`),
      // (inclusive3, _)
      makeTest(inclusive3, inclusive1)(`(inclusive3, inclusive1)`),
      makeTest(inclusive3, inclusive2)(`(inclusive3, inclusive2)`),
      makeTest(inclusive3, inclusive3)(`(inclusive3, inclusive3)`),
      makeTest(inclusive3, exclusive1)(`(inclusive3, exclusive1)`),
      makeTest(inclusive3, exclusive2)(`(inclusive3, exclusive2)`),
      makeTest(inclusive3, exclusive3)(`(inclusive3, exclusive3)`),
      // (exclusive1, _)
      makeTest(exclusive1, inclusive1)(`(exclusive1, inclusive1)`),
      makeTest(exclusive1, inclusive2)(`(exclusive1, inclusive2)`),
      makeTest(exclusive1, inclusive3)(`(exclusive1, inclusive3)`),
      makeTest(exclusive1, exclusive1)(`(exclusive1, exclusive1)`),
      makeTest(exclusive1, exclusive2)(`(exclusive1, exclusive2)`),
      makeTest(exclusive1, exclusive3)(`(exclusive1, exclusive3)`),
      // (exclusive2, _)
      makeTest(exclusive2, inclusive1)(`(exclusive2, inclusive1)`),
      makeTest(exclusive2, inclusive2)(`(exclusive2, inclusive2)`),
      makeTest(exclusive2, inclusive3)(`(exclusive2, inclusive3)`),
      makeTest(exclusive2, exclusive1)(`(exclusive2, exclusive1)`),
      makeTest(exclusive2, exclusive2)(`(exclusive2, exclusive2)`),
      makeTest(exclusive2, exclusive3)(`(exclusive2, exclusive3)`),
      // (exclusive3, _)
      makeTest(exclusive3, inclusive1)(`(exclusive3, inclusive1)`),
      makeTest(exclusive3, inclusive2)(`(exclusive3, inclusive2)`),
      makeTest(exclusive3, inclusive3)(`(exclusive3, inclusive3)`),
      makeTest(exclusive3, exclusive1)(`(exclusive3, exclusive1)`),
      makeTest(exclusive3, exclusive2)(`(exclusive3, exclusive2)`),
      makeTest(exclusive3, exclusive3)(`(exclusive3, exclusive3)`),
    )
  }

  override def testSpec: TestSpec =
    suite("InfiniteSetSpec")(
      binOpSuite("|")(_ | _)(
        // (inclusive1, _)
        `(inclusive1, inclusive1)` = Inclusive(A, B),
        `(inclusive1, inclusive2)` = Inclusive(A, B, C),
        `(inclusive1, inclusive3)` = Inclusive(A, B, C, D),
        `(inclusive1, exclusive1)` = Exclusive(),
        `(inclusive1, exclusive2)` = Exclusive(C),
        `(inclusive1, exclusive3)` = Exclusive(C, D),
        // (inclusive2, _)
        `(inclusive2, inclusive1)` = Inclusive(A, B, C),
        `(inclusive2, inclusive2)` = Inclusive(B, C),
        `(inclusive2, inclusive3)` = Inclusive(B, C, D),
        `(inclusive2, exclusive1)` = Exclusive(A),
        `(inclusive2, exclusive2)` = Exclusive(),
        `(inclusive2, exclusive3)` = Exclusive(D),
        // (inclusive3, _)
        `(inclusive3, inclusive1)` = Inclusive(A, B, C, D),
        `(inclusive3, inclusive2)` = Inclusive(B, C, D),
        `(inclusive3, inclusive3)` = Inclusive(C, D),
        `(inclusive3, exclusive1)` = Exclusive(A, B),
        `(inclusive3, exclusive2)` = Exclusive(B),
        `(inclusive3, exclusive3)` = Exclusive(),
        // (exclusive1, _)
        `(exclusive1, inclusive1)` = Exclusive(),
        `(exclusive1, inclusive2)` = Exclusive(A),
        `(exclusive1, inclusive3)` = Exclusive(A, B),
        `(exclusive1, exclusive1)` = Exclusive(A, B),
        `(exclusive1, exclusive2)` = Exclusive(B),
        `(exclusive1, exclusive3)` = Exclusive(),
        // (exclusive2, _)
        `(exclusive2, inclusive1)` = Exclusive(C),
        `(exclusive2, inclusive2)` = Exclusive(),
        `(exclusive2, inclusive3)` = Exclusive(B),
        `(exclusive2, exclusive1)` = Exclusive(B),
        `(exclusive2, exclusive2)` = Exclusive(B, C),
        `(exclusive2, exclusive3)` = Exclusive(C),
        // (exclusive3, _)
        `(exclusive3, inclusive1)` = Exclusive(C, D),
        `(exclusive3, inclusive2)` = Exclusive(D),
        `(exclusive3, inclusive3)` = Exclusive(),
        `(exclusive3, exclusive1)` = Exclusive(),
        `(exclusive3, exclusive2)` = Exclusive(C),
        `(exclusive3, exclusive3)` = Exclusive(C, D),
      ),
      binOpSuite("&")(_ & _)(
        // (inclusive1, _)
        `(inclusive1, inclusive1)` = Inclusive(A, B),
        `(inclusive1, inclusive2)` = Inclusive(B),
        `(inclusive1, inclusive3)` = Inclusive(),
        `(inclusive1, exclusive1)` = Inclusive(),
        `(inclusive1, exclusive2)` = Inclusive(A),
        `(inclusive1, exclusive3)` = Inclusive(A, B),
        // (inclusive2, _)
        `(inclusive2, inclusive1)` = Inclusive(B),
        `(inclusive2, inclusive2)` = Inclusive(B, C),
        `(inclusive2, inclusive3)` = Inclusive(C),
        `(inclusive2, exclusive1)` = Inclusive(C),
        `(inclusive2, exclusive2)` = Inclusive(),
        `(inclusive2, exclusive3)` = Inclusive(B),
        // (inclusive3, _)
        `(inclusive3, inclusive1)` = Inclusive(),
        `(inclusive3, inclusive2)` = Inclusive(C),
        `(inclusive3, inclusive3)` = Inclusive(C, D),
        `(inclusive3, exclusive1)` = Inclusive(C, D),
        `(inclusive3, exclusive2)` = Inclusive(D),
        `(inclusive3, exclusive3)` = Inclusive(),
        // (exclusive1, _)
        `(exclusive1, inclusive1)` = Inclusive(),
        `(exclusive1, inclusive2)` = Inclusive(C),
        `(exclusive1, inclusive3)` = Inclusive(C, D),
        `(exclusive1, exclusive1)` = Exclusive(A, B),
        `(exclusive1, exclusive2)` = Exclusive(A, B, C),
        `(exclusive1, exclusive3)` = Exclusive(A, B, C, D),
        // (exclusive2, _)
        `(exclusive2, inclusive1)` = Inclusive(A),
        `(exclusive2, inclusive2)` = Inclusive(),
        `(exclusive2, inclusive3)` = Inclusive(D),
        `(exclusive2, exclusive1)` = Exclusive(A, B, C),
        `(exclusive2, exclusive2)` = Exclusive(B, C),
        `(exclusive2, exclusive3)` = Exclusive(B, C, D),
        // (exclusive3, _)
        `(exclusive3, inclusive1)` = Inclusive(A, B),
        `(exclusive3, inclusive2)` = Inclusive(B),
        `(exclusive3, inclusive3)` = Inclusive(),
        `(exclusive3, exclusive1)` = Exclusive(A, B, C, D),
        `(exclusive3, exclusive2)` = Exclusive(B, C, D),
        `(exclusive3, exclusive3)` = Exclusive(C, D),
      ),
      binOpSuite("&~")(_ &~ _)(
        // (inclusive1, _)
        `(inclusive1, inclusive1)` = Inclusive(),
        `(inclusive1, inclusive2)` = Inclusive(A),
        `(inclusive1, inclusive3)` = Inclusive(A, B),
        `(inclusive1, exclusive1)` = Inclusive(A, B),
        `(inclusive1, exclusive2)` = Inclusive(B),
        `(inclusive1, exclusive3)` = Inclusive(),
        // (inclusive2, _)
        `(inclusive2, inclusive1)` = Inclusive(C),
        `(inclusive2, inclusive2)` = Inclusive(),
        `(inclusive2, inclusive3)` = Inclusive(B),
        `(inclusive2, exclusive1)` = Inclusive(B),
        `(inclusive2, exclusive2)` = Inclusive(B, C),
        `(inclusive2, exclusive3)` = Inclusive(C),
        // (inclusive3, _)
        `(inclusive3, inclusive1)` = Inclusive(C, D),
        `(inclusive3, inclusive2)` = Inclusive(D),
        `(inclusive3, inclusive3)` = Inclusive(),
        `(inclusive3, exclusive1)` = Inclusive(),
        `(inclusive3, exclusive2)` = Inclusive(C),
        `(inclusive3, exclusive3)` = Inclusive(C, D),
        // (exclusive1, _)
        `(exclusive1, inclusive1)` = Exclusive(A, B),
        `(exclusive1, inclusive2)` = Exclusive(A, B, C),
        `(exclusive1, inclusive3)` = Exclusive(A, B, C, D),
        `(exclusive1, exclusive1)` = Inclusive(),
        `(exclusive1, exclusive2)` = Inclusive(C),
        `(exclusive1, exclusive3)` = Inclusive(C, D),
        // (exclusive2, _)
        `(exclusive2, inclusive1)` = Exclusive(A, B, C),
        `(exclusive2, inclusive2)` = Exclusive(B, C),
        `(exclusive2, inclusive3)` = Exclusive(B, C, D),
        `(exclusive2, exclusive1)` = Inclusive(A),
        `(exclusive2, exclusive2)` = Inclusive(),
        `(exclusive2, exclusive3)` = Inclusive(D),
        // (exclusive3, _)
        `(exclusive3, inclusive1)` = Exclusive(A, B, C, D),
        `(exclusive3, inclusive2)` = Exclusive(B, C, D),
        `(exclusive3, inclusive3)` = Exclusive(C, D),
        `(exclusive3, exclusive1)` = Inclusive(A, B),
        `(exclusive3, exclusive2)` = Inclusive(B),
        `(exclusive3, exclusive3)` = Inclusive(),
      ),
    )

}
