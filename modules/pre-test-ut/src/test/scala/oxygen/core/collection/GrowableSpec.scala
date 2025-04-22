package oxygen.core.collection

import oxygen.predef.test.*

object GrowableSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("GrowableSpec")(
      test("Empty") {
        assertTrue(
          Growable.empty.toContiguous eq Contiguous.Empty,
        )
      },
      test("Single") {
        assertTrue(
          Growable.single(1).toContiguous == Contiguous.single(1),
          Growable.single(1).toContiguous.toList == (1 :: Nil),
        )
      },
      test("Many") {
        assertTrue(
          Growable.many(List(1, 2, 3)).toContiguous == Contiguous(1, 2, 3),
        )
      },
      test("Concat") {
        assertTrue(
          (Growable.many(List(-1, 0)) ++: (Growable.empty[Int] :++ Growable.single(1) :++ Growable.many(List(2, 3, 4)))).toContiguous ==
            Contiguous(-1, 0, 1, 2, 3, 4),
        )
      },
    )

}
