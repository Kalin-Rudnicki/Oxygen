package oxygen.core.syntax

import oxygen.core.typeclass.Monad

object monad {

  extension [F[_], A](self: F[A]) {

    def flatMap[B](f: A => F[B])(using monad: Monad[F]): F[B] =
      monad.flatMap(self)(f)

  }

}
