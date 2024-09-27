package oxygen.core.syntax

import oxygen.core.typeclass.NonEmpty

object nonEmpty {

  extension [F[_], A](self: F[A]) {

    def toNonEmpty(implicit nonEmpty: NonEmpty[F]): Option[nonEmpty.G[A]] = nonEmpty.nonEmpty(self)

    def unsafeToNonEmpty(implicit nonEmpty: NonEmpty[F]): nonEmpty.G[A] = nonEmpty.unsafeNonEmpty(self)

  }

}
