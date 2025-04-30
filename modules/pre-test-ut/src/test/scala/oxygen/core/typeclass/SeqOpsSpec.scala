package oxygen.core.typeclass

import oxygen.core.collection.Contiguous
import oxygen.predef.test.*

object SeqOpsSpec extends OxygenSpecDefault {

  private def transformSpec[F[_], G[_], A](f: F[A], g: G[A])(using fOps: SeqOps[F], gOps: SeqOps[G])(using SourceLocation): TestSpec =
    test(s"$f <-> $g") {
      assertTrue(
        f.transformTo[G] == g,
        g.transformTo[F] == f,
      )
    }

  override def testSpec: TestSpec =
    suite("SeqOpsSpec")(
      transformSpec(List(1, 2, 3), Vector(1, 2, 3)),
      transformSpec(Seq(1, 2, 3), Contiguous(1, 2, 3)),
      transformSpec(List.empty[Int], IndexedSeq.empty[Int]),
      transformSpec(Iterable(1, 2, 3), Seq(1, 2, 3)),
      transformSpec(List(1, 2, 3), Contiguous(1, 2, 3)),
    )

}
