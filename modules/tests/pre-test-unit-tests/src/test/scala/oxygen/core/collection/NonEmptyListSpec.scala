package oxygen.core.collection

import oxygen.core.syntax.option.*
import oxygen.test.*
import scala.collection.immutable.ArraySeq
import scala.util.control.TailCalls.{done, tailcall, TailRec}
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
            NonEmptyList.of(1, 2, 3) :++ ArraySeq(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            ArraySeq(1, 2, 3) ++: NonEmptyList.of(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(1, 2, 3) ++ ArraySeq(4, 5, 6) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(1, 2, 3).appendedAll(ArraySeq(4, 5, 6)) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
            NonEmptyList.of(4, 5, 6).prependedAll(Vector(1, 2, 3)) == NonEmptyList.of(1, 2, 3, 4, 5, 6),
          )
        },
      ),
      suite("flatMap")(
        test("basic flatMap produces correct result") {
          val nel = NonEmptyList.of(1, 2, 3)
          val result = nel.flatMap(x => NonEmptyList.of(x, x * 10))

          assertTrue(result == NonEmptyList.of(1, 10, 2, 20, 3, 30))
        },
        test("flatMap with single-element results") {
          val nel = NonEmptyList.of(1, 2, 3)
          val result = nel.flatMap(x => NonEmptyList.one(x * 2))

          assertTrue(result == NonEmptyList.of(2, 4, 6))
        },
        test("flatMap on single-element list") {
          val nel = NonEmptyList.one(5)
          val result = nel.flatMap(x => NonEmptyList.of(x, x + 1, x + 2))

          assertTrue(result == NonEmptyList.of(5, 6, 7))
        },
        test("flatMap preserves order") {
          val nel = NonEmptyList.of("a", "b")
          val result = nel.flatMap(s => NonEmptyList.of(s + "1", s + "2"))

          assertTrue(result == NonEmptyList.of("a1", "a2", "b1", "b2"))
        },
        test("left identity: flatMap(pure(a))(f) == f(a)") {
          val a = 42
          val f: Int => NonEmptyList[Int] = x => NonEmptyList.of(x, x * 2)
          val left = NonEmptyList.one(a).flatMap(f)
          val right = f(a)

          assertTrue(left == right)
        },
        test("right identity: m.flatMap(pure) == m") {
          val m = NonEmptyList.of(1, 2, 3)
          val result = m.flatMap(NonEmptyList.one)

          assertTrue(result == m)
        },
        test("associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))") {
          val m = NonEmptyList.of(1, 2)
          val f: Int => NonEmptyList[Int] = x => NonEmptyList.of(x, x + 1)
          val g: Int => NonEmptyList[Int] = x => NonEmptyList.of(x * 10)
          val left = m.flatMap(f).flatMap(g)
          val right = m.flatMap(x => f(x).flatMap(g))

          assertTrue(left == right)
        },
        test("stack-safe for large list iteration") {
          val largeList = NonEmptyList.unsafeFromList((1 to 50000).toList)
          val result = largeList.flatMap(x => NonEmptyList.one(x * 2))

          assertTrue(
            result.head == 2,
            result.last == 100000,
            result.size == 50000,
          )
        },
        test("stack-safe when f returns large results") {
          val nel = NonEmptyList.unsafeFromList((1 to 500).toList)
          val result = nel.flatMap(x => NonEmptyList.unsafeFromList((1 to 100).map(_ + x).toList))

          assertTrue(result.size == 50000)
        },
        test("chained flatMaps work correctly") {
          val nel = NonEmptyList.of(1, 2)
          val result = nel
            .flatMap(x => NonEmptyList.of(x, x + 10))
            .flatMap(x => NonEmptyList.of(x, x + 100))

          assertTrue(
            result == NonEmptyList.of(1, 101, 11, 111, 2, 102, 12, 112),
          )
        },
        test("many chained flatMaps via foldLeft") {
          val nel = NonEmptyList.one(0)
          val result = (1 to 1000).foldLeft(nel) { (acc, _) =>
            acc.flatMap(x => NonEmptyList.one(x + 1))
          }

          assertTrue(result == NonEmptyList.one(1000))
        },
      ),
      suite("flatMapTrampoline")(
        test("basic correctness matches flatMap") {
          val nel = NonEmptyList.of(1, 2, 3)
          val f: Int => NonEmptyList[Int] = x => NonEmptyList.of(x, x * 10)
          val regular = nel.flatMap(f)
          val trampolined = nel.flatMapTrampoline(x => done(f(x))).result
          assertTrue(regular == trampolined)
        },
        test("stack-safe for deep recursive monadic composition") {
          // NOTE: kalin, this function is what blows the stack
          // with the existing implementation
          def countdown(n: Int): TailRec[NonEmptyList[Int]] =
            if n <= 0 then done(NonEmptyList.one(0))
            else
              tailcall(countdown(n - 1)).flatMap { nel =>
                done(nel.flatMap(x => NonEmptyList.one(x + 1)))
              }

          val blowsTheStackAt10k = 20000
          val result = countdown(blowsTheStackAt10k).result

          assertTrue(result == NonEmptyList.one(blowsTheStackAt10k))
        },
        test("stack-safe for deeply nested flatMapTrampoline chains") {
          def nested(n: Int, nel: NonEmptyList[Int]): TailRec[NonEmptyList[Int]] =
            if n <= 0 then done(nel)
            else
              nel.flatMapTrampoline { x =>
                tailcall(nested(n - 1, NonEmptyList.one(x + 1)))
              }

          val reallyBigN = 10000
          val result = nested(reallyBigN, NonEmptyList.one(0)).result

          assertTrue(result == NonEmptyList.one(reallyBigN))
        },
      ),
    )
}
