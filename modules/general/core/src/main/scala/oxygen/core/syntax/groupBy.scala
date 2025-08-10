package oxygen.core.syntax

import oxygen.core.typeclass.NonEmpty
import scala.collection.IterableOps

object groupBy {

  extension [F[_], A](self: IterableOps[A, F, F[A]]) {

    def groupByNE[B](key: A => B)(implicit nonEmpty: NonEmpty[F]): Map[B, nonEmpty.G[A]] =
      self.groupBy(key).map { case (k, vs) => (k, nonEmpty.unsafeNonEmpty(vs)) }

    def groupMapNE[B, C](key: A => B)(value: A => C)(implicit nonEmpty: NonEmpty[F]): Map[B, nonEmpty.G[C]] =
      self.groupMap(key)(value).map { case (k, vs) => (k, nonEmpty.unsafeNonEmpty(vs)) }

  }

}
