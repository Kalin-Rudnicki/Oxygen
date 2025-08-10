package oxygen.core.syntax

import oxygen.core.typeclass.Functor

object functor {

  extension [F[_], A](self: F[A]) {

    def map[B](f: A => B)(using functor: Functor[F]): F[B] =
      functor.map(self)(f)

  }

}
