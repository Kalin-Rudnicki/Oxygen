package oxygen.core.typeclass

trait Functor[F[_]] {

  def map[A, B](self: F[A])(f: A => B): F[B]

}
object Functor extends FunctorLowPriority.LowPriority1 {

  inline def apply[F[_]](implicit ev: Functor[F]): ev.type = ev

}

object FunctorLowPriority {

  trait LowPriority1 {

    given fromApplicative: [F[_]] => (applicative: Applicative[F]) => Functor[F] = applicative

  }

}
