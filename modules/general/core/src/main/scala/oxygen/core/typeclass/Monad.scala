package oxygen.core.typeclass

import oxygen.core.RightProjection
import oxygen.core.collection.{Contiguous, NonEmptyList}
import oxygen.core.syntax.either.*
import oxygen.core.syntax.option.*

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](self: F[A])(f: A => F[B]): F[B]

}
object Monad extends MonadLowPriority.LowPriority1 {

  inline def apply[F[_]](implicit ev: Monad[F]): ev.type = ev

  given option: Monad[Option] =
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

  given nonEmptyList: Monad[NonEmptyList] =
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

  given either: [Left] => Monad[RightProjection[Left]] =
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

  implicit val iArray: Monad[IArray] =
    new Monad[IArray] {
      import Contiguous.typedAnyTag

      override def flatMap[A, B](self: IArray[A])(f: A => IArray[B]): IArray[B] = self.flatMap(f(_))

      override def ap[A, B](f: IArray[A => B])(self: IArray[A]): IArray[B] =
        for {
          f <- f
          self <- self
        } yield f(self)

      override def map[A, B](self: IArray[A])(f: A => B): IArray[B] =
        self.map(f)

      override def pure[A](self: A): IArray[A] =
        IArray(self)

    }

  implicit val contiguous: Monad[Contiguous] =
    new Monad[Contiguous] {

      override def map[A, B](self: Contiguous[A])(f: A => B): Contiguous[B] = self.map(f)

      override def flatMap[A, B](self: Contiguous[A])(f: A => Contiguous[B]): Contiguous[B] = self.flatMap(f)

      override def ap[A, B](f: Contiguous[A => B])(self: Contiguous[A]): Contiguous[B] =
        for {
          f <- f
          self <- self
        } yield f(self)

      override def pure[A](self: A): Contiguous[A] =
        Contiguous.single(self)

    }

}

object MonadLowPriority {

  trait LowPriority1 {

    given fromSeq: [F[_]] => (fOps: SeqOps[F]) => Monad[F] =
      new Monad[F] {

        override def map[A, B](self: F[A])(f: A => B): F[B] = {
          val iterator = fOps.newIterator(self)
          val builder = fOps.newBuilder[B]

          builder.sizeHint(fOps.knownSize(self))
          while (iterator.hasNext)
            builder.addOne(f(iterator.next()))

          builder.result()
        }

        override def pure[A](self: A): F[A] = {
          val builder = fOps.newBuilder[A]
          builder.addOne(self)
          builder.result()
        }

        override def ap[A, B](f: F[A => B])(self: F[A]): F[B] =
          flatMap(f) { f => map(self) { f(_) } }

        override def flatMap[A, B](self: F[A])(f: A => F[B]): F[B] = {
          val iterator = fOps.newIterator(self)
          val builder = fOps.newBuilder[B]

          while (iterator.hasNext)
            builder.addAll(fOps.newIterator(f(iterator.next())))

          builder.result()
        }

      }

  }

}
