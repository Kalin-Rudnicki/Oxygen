package oxygen.core.typeclass

import oxygen.core.RightProjection
import oxygen.core.syntax.either.*

trait ParTraverse[F[_], G[_]] {

  def parTraverse[A, B](self: F[A])(f: A => G[B]): G[F[B]]

  final def parSequence[A](self: F[G[A]]): G[F[A]] = parTraverse(self)(identity)

}
object ParTraverse extends ParTraverseLowPriority.LowPriority1 {

  inline def apply[F[_], G[_]](implicit ev: ParTraverse[F, G]): ev.type = ev

}

object ParTraverseLowPriority {

  trait LowPriority1 {

    implicit def functorEither[F[_], Left](implicit functor: Functor[F], semigroup: Semigroup[Left]): ParTraverse[F, RightProjection[Left]] =
      new ParTraverse[F, RightProjection[Left]] {

        override def parTraverse[A, B](self: F[A])(f: A => Either[Left, B]): Either[Left, F[B]] = {
          var left: Left = null.asInstanceOf[Left]

          val result =
            functor
              .map(self) { a =>
                f(a) match {
                  case Right(value) => value
                  case Left(value)  =>
                    left =
                      if (left == null) value
                      else semigroup.combine(left, value)

                    null.asInstanceOf[B]
                }
              }

          if (left == null) result.asRight
          else left.asLeft
        }

      }

  }

}
