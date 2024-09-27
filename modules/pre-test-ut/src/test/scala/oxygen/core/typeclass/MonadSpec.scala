package oxygen.core.typeclass

import oxygen.predef.core.*
import oxygen.test.*
import zio.test.*

object MonadSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("FunctorSpec")(
      suite("map")(
        suite("option")(
          test("some->some") {
            assertTrue(Monad[Option].flatMap(1.some)(_.some) == 1.some)
          },
          test("some->none") {
            assertTrue(Monad[Option].flatMap(Option.empty[Int])(_ => None) == None)
          },
          test("none->some") {
            assertTrue(Monad[Option].flatMap(Option.empty[Int])(_.some) == None)
          },
          test("none->none") {
            assertTrue(Monad[Option].flatMap(Option.empty[Int])(_ => None) == None)
          },
        ),
        test("list") {
          assertTrue(Monad[List].flatMap(List(1, 2, 3))(a => List.fill(a)(a)) == List(1, 2, 2, 3, 3, 3))
        },
        test("seq") {
          assertTrue(Monad[Seq].flatMap(Seq(1, 2, 3))(a => Seq.fill(a)(a)) == Seq(1, 2, 2, 3, 3, 3))
        },
        test("nel") {
          assertTrue(Monad[NonEmptyList].flatMap(NonEmptyList.of(1, 2, 3))(a => NonEmptyList.unsafeFill(a)(a)) == NonEmptyList.of(1, 2, 2, 3, 3, 3))
        },
      ),
    )

}
