package oxygen.meta

import scala.quoted.*

trait ExprMonad[F[_]] {

  def map[A, B](a: Expr[F[A]])(f: Expr[A => B])(using quotes: Quotes, fType: Type[F], aType: Type[A], bType: Type[B]): Expr[F[B]]

  def pure[A](a: Expr[A])(using quotes: Quotes, fType: Type[F], aType: Type[A]): Expr[F[A]]

  def flatMap[A, B](a: Expr[F[A]])(f: Expr[A => F[B]])(using quotes: Quotes, fType: Type[F], aType: Type[A], bType: Type[B]): Expr[F[B]]

  final def mapE[A, B](a: Expr[F[A]])(f: Expr[A] => Expr[B])(using quotes: Quotes, fType: Type[F], aType: Type[A], bType: Type[B]): Expr[F[B]] =
    map[A, B](a)('{ (a: A) => ${ f('a) } })

  final def flatMapE[A, B](a: Expr[F[A]])(f: Expr[A] => Expr[F[B]])(using quotes: Quotes, fType: Type[F], aType: Type[A], bType: Type[B]): Expr[F[B]] =
    flatMap[A, B](a)('{ (a: A) => ${ f('a) } })

}
object ExprMonad {

  given option: ExprMonad[Option] =
    new ExprMonad[Option] {

      override def map[A, B](a: Expr[Option[A]])(f: Expr[A => B])(using quotes: Quotes, fType: Type[Option], aType: Type[A], bType: Type[B]): Expr[Option[B]] =
        '{ $a.map($f) }

      override def pure[A](a: Expr[A])(using quotes: Quotes, fType: Type[Option], aType: Type[A]): Expr[Option[A]] =
        '{ Some($a) }

      override def flatMap[A, B](a: Expr[Option[A]])(f: Expr[A => Option[B]])(using quotes: Quotes, fType: Type[Option], aType: Type[A], bType: Type[B]): Expr[Option[B]] =
        '{ $a.flatMap($f) }

    }

  type EitherT[L] = [R] =>> Either[L, R]

  given either: [L: Type] => ExprMonad[EitherT[L]] =
    new ExprMonad[EitherT[L]] {

      override def map[A, B](a: Expr[Either[L, A]])(f: Expr[A => B])(using quotes: Quotes, fType: Type[EitherT[L]], aType: Type[A], bType: Type[B]): Expr[Either[L, B]] =
        '{ $a.map($f) }

      override def pure[A](a: Expr[A])(using quotes: Quotes, fType: Type[EitherT[L]], aType: Type[A]): Expr[Either[L, A]] =
        '{ Right($a) }

      override def flatMap[A, B](a: Expr[Either[L, A]])(f: Expr[A => Either[L, B]])(using quotes: Quotes, fType: Type[EitherT[L]], aType: Type[A], bType: Type[B]): Expr[Either[L, B]] =
        '{ $a.flatMap($f) }

    }

}
