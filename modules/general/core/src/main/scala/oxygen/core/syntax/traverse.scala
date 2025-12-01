package oxygen.core.syntax

import oxygen.core.typeclass.{ParTraverse, Traverse}

object traverse {

  extension [F[_], A](self: F[A]) {

    def traverse[G[_], B](f: A => G[B])(using traverse: Traverse[F, G]): G[F[B]] = traverse.traverse(self)(f)

    def parTraverse[G[_], B](f: A => G[B])(using parTraverse: ParTraverse[F, G]): G[F[B]] = parTraverse.parTraverse(self)(f)

  }

  extension [F[_], G[_], A](self: F[G[A]]) {

    def sequence(using traverse: Traverse[F, G]): G[F[A]] = traverse.sequence(self)

    def parSequence(using parTraverse: ParTraverse[F, G]): G[F[A]] = parTraverse.parSequence(self)

  }

}
