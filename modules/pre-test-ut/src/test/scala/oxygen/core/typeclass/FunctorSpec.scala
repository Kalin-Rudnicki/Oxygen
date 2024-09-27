package oxygen.core.typeclass

import oxygen.core.*
import oxygen.predef.core.*
import oxygen.test.*
import zio.test.*

object FunctorSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("FunctorSpec")(
      suite("map")(
        suite("option")(
          test("some") {
            assertTrue(Functor[Option].map(1.some)(_ + 1) == 2.some)
          },
          test("none") {
            assertTrue(Functor[Option].map(Option.empty[Int])(_ + 1) == None)
          },
        ),
        suite("either")(
          test("right") {
            assertTrue(Functor[RightProjection[String]].map(1.asRight)(_ + 1) == 2.asRight)
          },
          test("left") {
            assertTrue(Functor[RightProjection[String]].map("L".asLeft[Int])(_ + 1) == "L".asLeft)
          },
        ),
        suite("list")(
          test("non-empty") {
            assertTrue(Functor[List].map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
          },
          test("empty") {
            assertTrue(Functor[List].map(List.empty[Int])(_ + 1) == List())
          },
        ),
        suite("seq")(
          test("non-empty") {
            assertTrue(Functor[Seq].map(Seq(1, 2, 3))(_ + 1) == Seq(2, 3, 4))
          },
          test("empty") {
            assertTrue(Functor[Seq].map(Seq.empty[Int])(_ + 1) == Seq())
          },
        ),
        test("nel") {
          assertTrue(Functor[NonEmptyList].map(NonEmptyList.of(1, 2, 3))(_ + 1) == NonEmptyList.of(2, 3, 4))
        },
      ),
    )

}
