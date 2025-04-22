package oxygen.core.collection

import oxygen.predef.test.*

object ContiguousSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("ContiguousSpec")(
      suite("equals")(
        test("empty") {
          assertTrue(
            Contiguous() == Contiguous(),
          )
        },
        test("int, same tag") {
          assertTrue(
            Contiguous[Int](1, 2, 3) == Contiguous[Int](1, 2, 3),
          )
        },
        test("int, different tag") {
          assertTrue(
            Contiguous[Int](1, 2, 3) == Contiguous[Any](1, 2, 3),
          )
        },
        test("!= , different size") {
          assertTrue(
            Contiguous[Int](1, 2, 3) != Contiguous[Any](1, 2, 3, 4),
          )
        },
        test("!= , same size") {
          assertTrue(
            Contiguous[Int](1, 2, 3) != Contiguous[Any](1, 2, 5),
          )
        },
      ),
      suite("++")(
        test("same tag") {
          assertTrue(
            Contiguous[Int](1, 2, 3) ++ Contiguous[Int](4, 5, 6) == Contiguous[Int](1, 2, 3, 4, 5, 6),
          )
        },
        test("different tag") {
          assertTrue(
            Contiguous[Int](1, 2, 3) ++ Contiguous[Int](4, 5, 6) == Contiguous[Int](1, 2, 3, 4, 5, 6),
          )
        },
        test("different type") {
          val res = Contiguous[Int](1, 2, 3) ++ Contiguous[String]("4", "5", "6")
          assertTrue(
            res(0) == 1,
            res(1) == 2,
            res(2) == 3,
            res(3) == "4",
            res(4) == "5",
            res(5) == "6",
            res == Contiguous[Any](1, 2, 3, "4", "5", "6"),
          )
        },
        test("after type changing") {
          val a: Contiguous[Int] = Contiguous[Int](1, 2, 3)
          val b: Contiguous[Any] = a
          val c: Contiguous[Int] = Contiguous[Int](4, 5, 6)
          val d: Contiguous[Any] = b ++ c
          assertTrue(
            d == Contiguous(1, 2, 3, 4, 5, 6),
          )
        },
      ),
      suite("map")(
        test("simple") {
          assertTrue(
            Contiguous(1, 2, 3).map(a => a * a) == Contiguous(1, 4, 9),
          )
        },
      ),
      suite("flatMap")(
        test("simple") {
          assertTrue(
            Contiguous(1, 2, 3).flatMap { a => Contiguous.fill(a)(a) } == Contiguous(1, 2, 2, 3, 3, 3),
          )
        },
        test("flatMap to list") {
          assertTrue(
            Contiguous(1, 2, 3).flatMapIterable { a => List.fill(a)(a) } == Contiguous(1, 2, 2, 3, 3, 3),
          )
        },
      ),
      suite("partition")(
        test("partitionMap works") {
          assertTrue(
            Contiguous[String | Int]("1", 2, 3, "four").partitionMap {
              case string: String => string.asLeft
              case int: Int       => int.asRight
            } == (
              Contiguous("1", "four"),
              Contiguous(2, 3),
            ),
          )
        },
      ),
    )

}
