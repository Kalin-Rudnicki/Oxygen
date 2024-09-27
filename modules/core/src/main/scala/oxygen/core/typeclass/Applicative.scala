package oxygen.core.typeclass

trait Applicative[F[_]] extends Functor[F] with Pure[F] {

  def ap[A, B](f: F[A => B])(self: F[A]): F[B]

}
object Applicative extends ApplicativeLowPriority.LowPriority1 {

  inline def apply[F[_]](implicit ev: Applicative[F]): ev.type = ev

}

object ApplicativeLowPriority {

  trait LowPriority1 {

    implicit def fromMonad[F[_]](implicit monad: Monad[F]): Applicative[F] = monad

  }

}
