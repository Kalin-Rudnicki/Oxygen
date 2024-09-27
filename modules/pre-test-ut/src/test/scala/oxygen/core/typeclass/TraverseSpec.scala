package oxygen.core.typeclass

import oxygen.predef.core.*
import oxygen.test.*
import zio.test.*

object TraverseSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("TraverseSpec")(
      suite("travers")(
        suite("list/option")(
          test("case 1") {
            assertTrue(List(1, 2, 3).traverse(_.someWhen(_ > 0)) == List(1, 2, 3).some)
          },
          test("case 2") {
            assertTrue(List(0, 1, 2, 3).traverse(_.someWhen(_ > 0)) == None)
          },
        ),
        suite("nel/option")(
          test("case 1") {
            assertTrue(NonEmptyList.of(1, 2, 3).traverse(_.someWhen(_ > 0)) == NonEmptyList.of(1, 2, 3).some)
          },
          test("case 2") {
            assertTrue(NonEmptyList.of(0, 1, 2, 3).traverse(_.someWhen(_ > 0)) == None)
          },
        ),
        suite("list/either")(
          test("case 1") {
            assertTrue(List(1, 2, 3).traverse(_.rightWhenF(_ > 0)(identity)) == List(1, 2, 3).asRight)
          },
          test("case 2") {
            assertTrue(List(0, 1, 2, 3, -1).traverse(_.rightWhenF(_ > 0)(identity)) == 0.asLeft)
          },
          test("case 3") {
            assertTrue(List(1, 2, 3, -1).traverse(_.rightWhenF(_ > 0)(identity)) == -1.asLeft)
          },
        ),
        suite("nel/either")(
          test("case 1") {
            assertTrue(NonEmptyList.of(1, 2, 3).traverse(_.rightWhenF(_ > 0)(identity)) == NonEmptyList.of(1, 2, 3).asRight)
          },
          test("case 2") {
            assertTrue(NonEmptyList.of(0, 1, 2, 3, -1).traverse(_.rightWhenF(_ > 0)(identity)) == 0.asLeft)
          },
          test("case 3") {
            assertTrue(NonEmptyList.of(1, 2, 3, -1).traverse(_.rightWhenF(_ > 0)(identity)) == -1.asLeft)
          },
        ),
      ),
    )

}
