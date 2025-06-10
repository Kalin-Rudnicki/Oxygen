package oxygen.core.syntax

import oxygen.core.typeclass.{Functor, SeqOps}

object functor {

  extension [S[_], A](self: S[A]) {

    def map[B](f: A => B)(using seqOps: SeqOps[S]): S[B] =
      Functor[S].map(self)(f)

  }

}
