package oxygen.meta.typing

import oxygen.predef.test.*

object UnionRemovingSpec extends OxygenSpecDefault {

  final class UnionFilter[A, B, C] private (f: A => Either[B, C]) {

    def toEither(value: A): Either[B, C] = f(value)

    def filter[Removing](using ev: UnionRemoving[C, Removing]): UnionFilter[A, B | ev.Removing, ev.Remaining] =
      UnionFilter[A, B | ev.Removing, ev.Remaining] { f(_).flatMap(ev.apply) }

  }
  object UnionFilter {
    def initial[A]: UnionFilter[A, Nothing, A] = UnionFilter(_.asRight)
  }

  sealed trait T12
  sealed trait T34

  case object T1 extends T12
  case object T2 extends T12
  case object T3 extends T34
  case object T4 extends T34

  type T1 = T1.type
  type T2 = T2.type
  type T3 = T3.type
  type T4 = T4.type

  type All = T1 | T2 | T3 | T4

  private def makeTest[A, B, C](value: A, filter: UnionFilter[A, Nothing, A] => UnionFilter[A, B, C])(exp: Either[B, C])(using t: Trace, loc: SourceLocation): TestSpec =
    test(s"Test on line #${loc.line}") {
      assertTrue(filter(UnionFilter.initial[A]).toEither(value) == exp)
    }
  private def makeAllTest[B, C](value: All, filter: UnionFilter[All, Nothing, All] => UnionFilter[All, B, C])(exp: Either[B, C])(using Trace, SourceLocation): TestSpec =
    makeTest(value, filter)(exp)

  val typed1: UnionFilter[T1 | T2 | T3 | T4, Nothing, T1 | T2 | T3 | T4] = UnionFilter.initial[All]
  val typed2: UnionFilter[T1 | T2 | T3 | T4, T1, T2 | T3 | T4] = UnionFilter.initial[All].filter[T1]
  val typed3: UnionFilter[T1 | T2 | T3 | T4, T1 | T2, T3 | T4] = UnionFilter.initial[All].filter[T1].filter[T2]
  val typed4: UnionFilter[T1 | T2 | T3 | T4, T1 | T2 | T3, T4] = UnionFilter.initial[All].filter[T1].filter[T2].filter[T3]
  val typed5: UnionFilter[T1 | T2 | T3 | T4, T1 | T2 | T3 | T4, Nothing] = UnionFilter.initial[All].filter[T1].filter[T2].filter[T3].filter[T4]

  val raw1: UnionRemoving.Aux[T1 | T2 | T3 | T4, T1, T2 | T3 | T4] = UnionRemoving.derived[T1 | T2 | T3 | T4, T1]
  val raw2: UnionRemoving.Aux[T1 | T2 | T3 | T4, T1 | T2, T3 | T4] = UnionRemoving.derived[T1 | T2 | T3 | T4, T1 | T2]
  val raw3: UnionRemoving.Aux[T1 | T2 | T3 | T4, T1 | T2 | T3, T4] = UnionRemoving.derived[T1 | T2 | T3 | T4, T1 | T2 | T3]
  val raw4: UnionRemoving.Aux[T1 | T2 | T3 | T4, T1 | T2 | T3 | T4, Nothing] = UnionRemoving.derived[T1 | T2 | T3 | T4, T1 | T2 | T3 | T4]

  override def testSpec: TestSpec =
    suite("UnionRemovingSpec")(
      suite("removes")(
        makeAllTest(T1, _.filter[All])(T1.asLeft),
        makeAllTest(T1, _.filter[T1])(T1.asLeft),
        makeAllTest(T1, _.filter[T1].filter[T2])(T1.asLeft),
        makeAllTest(T1, _.filter[T2].filter[T1])(T1.asLeft),
        makeAllTest(T1, _.filter[T12])(T1.asLeft),
      ),
      suite("remains")(
        makeAllTest(T1, identity)(T1.asRight),
        makeAllTest(T1, _.filter[T3])(T1.asRight),
        makeAllTest(T1, _.filter[T3].filter[T4])(T1.asRight),
        makeAllTest(T1, _.filter[T4].filter[T3])(T1.asRight),
        makeAllTest(T1, _.filter[T34])(T1.asRight),
      ),
    )

}
