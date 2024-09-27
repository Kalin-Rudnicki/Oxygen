package oxygen.core.typeclass

import scala.collection.IterableOnceOps

trait Functor[F[_]] {

  def map[A, B](self: F[A])(f: A => B): F[B]

}
object Functor extends FunctorLowPriority.LowPriority1 {

  inline def apply[F[_]](implicit ev: Functor[F]): ev.type = ev

}

object FunctorLowPriority {

  trait LowPriority1 extends LowPriority2 {

    implicit def fromApplicative[F[_]](implicit applicative: Applicative[F]): Applicative[F] = applicative

  }
  trait LowPriority2 {

    implicit def fromIterableOnceOps[F[A] <: IterableOnceOps[A, F, F[A]]]: Functor[F] =
      new Functor[F] {
        override def map[A, B](self: F[A])(f: A => B): F[B] = self.map(f)
      }

  }

}
