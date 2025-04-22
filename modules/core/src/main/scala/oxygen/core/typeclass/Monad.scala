package oxygen.core.typeclass

import oxygen.core.RightProjection
import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.either.*
import oxygen.core.syntax.option.*
import scala.collection.IterableOnceOps

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](self: F[A])(f: A => F[B]): F[B]

}
object Monad extends MonadLowPriority.LowPriority1 {

  inline def apply[F[_]](implicit ev: Monad[F]): ev.type = ev

  implicit val option: Monad[Option] =
    new Monad[Option] {

      override def flatMap[A, B](self: Option[A])(f: A => Option[B]): Option[B] = self.flatMap(f)

      override def ap[A, B](f: Option[A => B])(self: Option[A]): Option[B] =
        for {
          f <- f
          self <- self
        } yield f(self)

      override def map[A, B](self: Option[A])(f: A => B): Option[B] = self.map(f)

      override def pure[A](self: A): Option[A] = self.some

    }

  implicit val nonEmptyList: Monad[NonEmptyList] =
    new Monad[NonEmptyList] {
      override def flatMap[A, B](self: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] = self.flatMap(f)

      override def ap[A, B](f: NonEmptyList[A => B])(self: NonEmptyList[A]): NonEmptyList[B] =
        for {
          f <- f
          self <- self
        } yield f(self)

      override def map[A, B](self: NonEmptyList[A])(f: A => B): NonEmptyList[B] = self.map(f)

      override def pure[A](self: A): NonEmptyList[A] = NonEmptyList.one(self)

    }

  implicit def either[Left]: Monad[RightProjection[Left]] =
    new Monad[RightProjection[Left]] {

      override def flatMap[A, B](self: Either[Left, A])(f: A => Either[Left, B]): Either[Left, B] = self.flatMap(f)

      override def ap[A, B](f: Either[Left, A => B])(self: Either[Left, A]): Either[Left, B] =
        for {
          f <- f
          self <- self
        } yield f(self)

      override def map[A, B](self: Either[Left, A])(f: A => B): Either[Left, B] = self.map(f)

      override def pure[A](self: A): Either[Left, A] = self.asRight

    }

}

object MonadLowPriority {

  trait LowPriority1 {

    implicit def fromIterableOnceOps[F[A] <: IterableOnceOps[A, F, F[A]] & IterableOnce[A]](implicit _pure: Pure[F]): Monad[F] =
      new Monad[F] {

        override def flatMap[A, B](self: F[A])(f: A => F[B]): F[B] = self.flatMap(f)

        override def ap[A, B](f: F[A => B])(self: F[A]): F[B] =
          for {
            f <- f
            self <- self
          } yield f(self)

        override def map[A, B](self: F[A])(f: A => B): F[B] = self.map(f)

        override def pure[A](self: A): F[A] = _pure.pure(self)

      }

  }

}
