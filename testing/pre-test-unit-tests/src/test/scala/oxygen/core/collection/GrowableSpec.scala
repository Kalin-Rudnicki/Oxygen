package oxygen.core.collection

import oxygen.predef.test.*

object GrowableSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("GrowableSpec")(
      test("Empty") {
        assertTrue(
          Growable.empty.toContiguous == Contiguous.Empty,
        )
      },
      test("Single") {
        assertTrue(
          Growable.single(1).toContiguous == Contiguous.single(1),
          Growable.single(1).to[List] == (1 :: Nil),
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
      test("Map") {
        assertTrue(
          (Growable.many(List(-1, 0)) ++: (Growable.empty[Int] :++ Growable.single(1) :++ Growable.many(List(2, 3, 4)))).map(_.toString).toContiguous ==
            Contiguous("-1", "0", "1", "2", "3", "4"),
        )
      },
      test("Map") {
        assertTrue(
          (Growable.many(List(-1, 0)) ++: (Growable.empty[Int] :++ Growable.single(1) :++ Growable.many(List(2, 3, 4)))).map(_.toString).toContiguous ==
            Contiguous("-1", "0", "1", "2", "3", "4"),
        )
      },
      test("FlatMap + Fill") {
        assertTrue(
          Growable(1, 2, 3).flatMap { i => Growable.fill(i)(i) }.toContiguous ==
            Contiguous(1, 2, 2, 3, 3, 3),
        )
      },
      test("Collect") {
        assertTrue(
          Growable("ignore".asLeft, 1.asRight, "ignore".asLeft, 2.asRight, "ignore".asLeft, 3.asRight, "ignore".asLeft).collect { case Right(a) => a }.to[Vector] ==
            Vector(1, 2, 3),
        )
      },
      test("Filter") {
        assertTrue(
          Growable("ignore".asLeft, 1.asRight, "ignore".asLeft, 2.asRight, "ignore".asLeft, 3.asRight, "ignore".asLeft, 4.asRight, "ignore".asLeft).flatMap(_.toOption).filter(_ > 2).to[IndexedSeq] ==
            IndexedSeq(3, 4),
        )
      },
      test("FilterNot") {
        assertTrue(
          Growable("ignore".asLeft, 1.asRight, "ignore".asLeft, 2.asRight, "ignore".asLeft, 3.asRight, "ignore".asLeft, 4.asRight, "ignore".asLeft)
            .flatMap(_.toOption)
            .filterNot(_ > 2)
            .to[IndexedSeq] ==
            IndexedSeq(1, 2),
        )
      },
      test("Distinct") {
        assertTrue(
          Growable(1, 2, 3, 2, 4, 4, 5, 3, 6, 3, 7).distinct.to[List] ==
            List(1, 2, 3, 4, 5, 6, 7),
        )
      },
      test("DistinctBy") {
        assertTrue(
          Growable(1, 2, 3, 2, 4, 4, 5, 3, 6, 3, 7).distinctBy(_ % 2).to[List] ==
            List(1, 2),
        )
      },
    )

}
