package oxygen.core.typeclass

import oxygen.core.*
import oxygen.predef.core.*
import oxygen.test.*
import zio.test.*

object ApplicativeSpec extends OxygenSpecDefault {

  private val plusOne: Int => Int = _ + 1
  private val minusOne: Int => Int = _ - 1

  override def testSpec: TestSpec =
    suite("ApplicativeSpec")(
      suite("pure")(
        test("option") {
          assertTrue(Applicative[Option].pure(1) == 1.some)
        },
        test("either") {
          assertTrue(Applicative[RightProjection[String]].pure(1) == 1.asRight)
        },
        test("list") {
          assertTrue(Applicative[List].pure(1) == List(1))
        },
        test("seq") {
          assertTrue(Applicative[Seq].pure(1) == Seq(1))
        },
        test("nel") {
          assertTrue(Applicative[NonEmptyList].pure(1) == NonEmptyList.of(1))
        },
      ),
      suite("ap")(
        suite("option")(
          test("some/some") {
            assertTrue(Applicative[Option].ap(plusOne.some)(1.some) == 2.some)
          },
          test("none/some") {
            assertTrue(Applicative[Option].ap(None)(1.some) == None)
          },
          test("some/none") {
            assertTrue(Applicative[Option].ap(plusOne.some)(Option.empty[Int]) == None)
          },
          test("none/none") {
            assertTrue(Applicative[Option].ap(None)(Option.empty[Int]) == None)
          },
        ),
        suite("either")(
          test("right/right") {
            assertTrue(Applicative[RightProjection[String]].ap(plusOne.asRight)(1.asRight) == 2.asRight)
          },
          test("left/right") {
            assertTrue(Applicative[RightProjection[String]].ap("f".asLeft[Int => Int])(1.asRight) == "f".asLeft)
          },
          test("right/left") {
            assertTrue(Applicative[RightProjection[String]].ap(plusOne.asRight)("self".asLeft[Int]) == "self".asLeft)
          },
          test("left/left") {
            assertTrue(Applicative[RightProjection[String]].ap("f".asLeft)("self".asLeft[Int]) == "f".asLeft[Int])
          },
        ),
        suite("list")(
          test("non-empty/non-empty") {
            assertTrue(Applicative[List].ap(List(plusOne, minusOne))(List(10, 20)) == List(11, 21, 9, 19))
          },
          test("empty/non-empty") {
            assertTrue(Applicative[List].ap(List())(List(10, 20)) == List())
          },
          test("non-empty/empty") {
            assertTrue(Applicative[List].ap(List(plusOne, minusOne))(List()) == List())
          },
        ),
        test("nel") {
          assertTrue(Applicative[NonEmptyList].ap(NonEmptyList.of(plusOne, minusOne))(NonEmptyList.of(10, 20)) == NonEmptyList.of(11, 21, 9, 19))
        },
      ),
    )

}
