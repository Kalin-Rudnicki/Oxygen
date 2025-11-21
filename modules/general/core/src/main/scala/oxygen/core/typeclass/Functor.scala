package oxygen.core.typeclass

import scala.collection.immutable.ArraySeq

trait Functor[F[_]] {

  def map[A, B](self: F[A])(f: A => B): F[B]

}
object Functor extends FunctorLowPriority.LowPriority1 {

  inline def apply[F[_]](implicit ev: Functor[F]): ev.type = ev

  given arraySeq: Functor[ArraySeq] =
    new Functor[ArraySeq] {
      override def map[A, B](self: ArraySeq[A])(f: A => B): ArraySeq[B] = self.map(f)
    }

}

object FunctorLowPriority {

  trait LowPriority1 {

    given fromApplicative: [F[_]] => (applicative: Applicative[F]) => Functor[F] = applicative

  }

}
