package oxygen.core.collection

import oxygen.core.syntax.option.*
import oxygen.test.*
import zio.test.*

object NonEmptyListSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("NonEmptyListSpec")(
      test("of") {
        assertTrue(
          NonEmptyList.of(1, 2, 3, 4) == NonEmptyList(1, List(2, 3, 4)),
        )
      },
      suite("fromList")(
        test("non-empty") {
          assertTrue(NonEmptyList.fromList(List(1, 2, 3)) == NonEmptyList(1, List(2, 3)).some)
        },
        test("empty") {
          assertTrue(NonEmptyList.fromList(List()).isEmpty)
        },
      ),
      suite("unapply")(
        test("nel") {
          assertTrue(
            NonEmptyList.of(1, 2, 3, 4) match {
              case NonEmptyList(1, 2 :: 3 :: 4 :: Nil) => true
              case _                                   => false
            },
          )
        },
        test("list - non-empty") {
          assertTrue(
            List(1, 2, 3, 4) match {
              case NonEmptyList(1, 2 :: 3 :: 4 :: Nil) => true
              case _                                   => false
            },
          )
        },
        test("list - non-empty") {
          assertTrue(
            List() match {
              case NonEmptyList(_, _) => false
              case _                  => true
            },
          )
        },
      ),
      suite("fill")(
        // test("< 1 doesn't compile") {
        //   assertTrue(NonEmptyList.fill(0)(()) == ???)
        // },
        test("1") {
          assertTrue(NonEmptyList.fill(1)(()) == NonEmptyList.of(()))
        },
        test("5") {
          assertTrue(NonEmptyList.fill(5)(()) == NonEmptyList.of((), (), (), (), ()))
        },
      ),
      suite("growing")(
        test("concat ops work") {
          assertTrue(
            NonEmptyList.of(1, 2, 3) ::: NonEmptyList.of(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(1, 2, 3) :++ NonEmptyList.of(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(1, 2, 3) ++: NonEmptyList.of(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(1, 2, 3) ++ NonEmptyList.of(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
          )
        },
        test("concat ops via SeqLike works") {
          assertTrue(
            NonEmptyList.of(1, 2, 3) :++ Contiguous(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            Contiguous(1, 2, 3) ++: NonEmptyList.of(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(1, 2, 3) ++ Contiguous(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(1, 2, 3).appendedAll(Contiguous(4, 5, 6)) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(4, 5, 6).prependedAll(Vector(1, 2, 3)) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
          )
        },
      ),
    )

}
