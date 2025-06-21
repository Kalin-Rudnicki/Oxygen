package oxygen.sql.generic

import oxygen.core.typeclass.{Functor, Traverse}
import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] sealed trait ParseResult[+A] {

  def map[B](f: A => B): ParseResult[B]
  def flatMap[B](f: A => ParseResult[B]): ParseResult[B]

  final def toSuccess: Option[A] = this match
    case ParseResult.Success(value) => value.some
    case _: ParseResult.Error       => None
    case _: ParseResult.Unknown     => None

  final def toKnown: Option[ParseResult.Known[A]] = this match
    case known: ParseResult.Known[A] => known.some
    case _: ParseResult.Unknown      => None

  final def getOrReport(using Quotes): A = this match
    case ParseResult.Success(value)                     => value
    case ParseResult.Error(tree, attempting, message)   => report.errorAndAbort(s"Error attempting $attempting : $message\n\n" + tree.showDetailed("tree"))
    case ParseResult.Unknown(tree, attempting, message) => report.errorAndAbort(s"Unknown tree attempting $attempting : $message\n\n" + tree.showDetailed("tree"))

  final def toEither: Either[ParseResult.NotSuccess, A] = this match
    case ParseResult.Success(value)         => value.asRight
    case notSuccess: ParseResult.NotSuccess => notSuccess.asLeft

  final def orElse[A2 >: A](default: => A2): ParseResult.Known[A2] =
    this.toKnown.getOrElse(ParseResult.Success(default))

  final def unknownAsError: ParseResult.Known[A] = this match
    case success: ParseResult.Success[A]                => success
    case error: ParseResult.Error                       => error
    case ParseResult.Unknown(tree, attempting, message) => ParseResult.Error(tree, attempting, message)

  final def errorAsUnknown: ParseResult[A] = this match
    case success: ParseResult.Success[A]              => success
    case unknown: ParseResult.Unknown                 => unknown
    case ParseResult.Error(tree, attempting, message) => ParseResult.Unknown(tree, attempting, message)

}
private[generic] object ParseResult {

  given parseResultFunctor: [F[_]] => (functor: Functor[F]) => Traverse[F, ParseResult] =
    new Traverse[F, ParseResult] {
      override def traverse[A, B](self: F[A])(f: A => ParseResult[B]): ParseResult[F[B]] =
        ParseResult.fromEither(Traverse.functorEither[F, ParseResult.NotSuccess](using functor).traverse(self)(f(_).toEither))
    }

  def fromEither[A](either: Either[ParseResult.NotSuccess, A]): ParseResult[A] = either match
    case Right(value) => ParseResult.Success(value)
    case Left(value)  => value

  def validate(cond: Boolean)(tree: Tree, message: String)(using ParseContext): ParseResult.Known[Unit] =
    if (cond) ParseResult.Success(())
    else ParseResult.error(tree, message)

  sealed trait Known[+A] extends ParseResult[A] {

    override def map[B](f: A => B): ParseResult[B] = this match
      case ParseResult.Success(value) => ParseResult.Success(f(value))
      case error: ParseResult.Error   => error

    override def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = this match
      case ParseResult.Success(value) => f(value)
      case error: ParseResult.Error   => error

  }

  sealed trait NotSuccess extends ParseResult[Nothing]

  final case class Success[+A](value: A) extends ParseResult.Known[A]

  final case class Error(tree: Tree, attempting: ParseContext, message: String) extends ParseResult.Known[Nothing], NotSuccess

  final case class Unknown(tree: Tree, attempting: ParseContext, message: String) extends NotSuccess {
    override def map[B](f: Nothing => B): ParseResult[B] = this
    override def flatMap[B](f: Nothing => ParseResult[B]): ParseResult[B] = this
  }

  def success[A](value: A): ParseResult[A] = Success(value)
  def error(tree: Tree, message: String)(using attempting: ParseContext): Error = Error(tree, attempting, message)
  def unknown(tree: Tree, message: String)(using attempting: ParseContext): Unknown = Unknown(tree, attempting, message)

}
