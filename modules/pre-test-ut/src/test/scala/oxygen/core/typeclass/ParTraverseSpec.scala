package oxygen.core.typeclass

import oxygen.predef.core.*
import oxygen.test.*
import zio.test.*

object ParTraverseSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("ParTraverseSpec")(
      suite("parTraverse")(
        suite("list/either")(
          test("case 1") {
            assertTrue(List(1, 2, 3).parTraverse(_.rightWhenF(_ > 0)(_ :: Nil)) == List(1, 2, 3).asRight)
          },
          test("case 2") {
            assertTrue(List(0, 1, 2, 3, -1).parTraverse(_.rightWhenF(_ > 0)(_ :: Nil)) == List(0, -1).asLeft)
          },
          test("case 3") {
            assertTrue(List(1, 2, 3, 0).parTraverse(_.rightWhenF(_ > 0)(_ :: Nil)) == List(0).asLeft)
          },
        ),
        suite("nel/either")(
          test("case 1") {
            assertTrue(NonEmptyList.of(1, 2, 3).parTraverse(_.rightWhenF(_ > 0)(NonEmptyList.one(_))) == NonEmptyList.of(1, 2, 3).asRight)
          },
          test("case 2") {
            assertTrue(NonEmptyList.of(0, 1, 2, 3, -1).parTraverse(_.rightWhenF(_ > 0)(NonEmptyList.one(_))) == NonEmptyList.of(0, -1).asLeft)
          },
          test("case 3") {
            assertTrue(NonEmptyList.of(1, 2, 3, -1).parTraverse(_.rightWhenF(_ > 0)(NonEmptyList.one(_))) == NonEmptyList.of(-1).asLeft)
          },
        ),
      ),
    )

}
