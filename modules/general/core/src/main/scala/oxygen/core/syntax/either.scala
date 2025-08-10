package oxygen.core.syntax

import oxygen.core.EitherNel
import oxygen.core.collection.NonEmptyList

object either {

  extension [A](self: A) {

    inline def asRight[B]: Either[B, A] = Right(self)

    inline def asLeft[B]: Either[A, B] = Left(self)

    inline def asRightNel[B]: EitherNel[B, A] = Right(self)

    inline def asLeftNel[B]: EitherNel[A, B] = Left(NonEmptyList.one(self))

  }

  extension [A, B](self: Either[A, B]) {

    def leftMap[A2](f: A => A2): Either[A2, B] = self match
      case Right(value) => Right(value)
      case Left(value)  => Left(f(value))

    def bimap[A2, B2](left: A => A2, right: B => B2): Either[A2, B2] = self match {
      case Right(value) => Right(right(value))
      case Left(value)  => Left(left(value))
    }

  }

  extension [A](self: A) {

    def rightWhen[B](f: A => Boolean)(ifNot: B): Either[B, A] =
      if (f(self)) self.asRight
      else ifNot.asLeft

    def rightWhenF[B](f: A => Boolean)(ifNot: A => B): Either[B, A] =
      if (f(self)) self.asRight
      else ifNot(self).asLeft

    def leftWhen[B](f: A => Boolean)(ifNot: B): Either[A, B] =
      if (f(self)) self.asLeft
      else ifNot.asRight

    def leftWhenF[B](f: A => Boolean)(ifNot: A => B): Either[A, B] =
      if (f(self)) self.asLeft
      else ifNot(self).asRight

  }

}
