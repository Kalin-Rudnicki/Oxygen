package oxygen.http.model.internal

import oxygen.http.core.*
import oxygen.predef.core.*
import zio.*

sealed trait IntermediateRequestParseResult[+A] {

  def map[B](f: A => B): IntermediateRequestParseResult[B]
  def flatMap[B](f: (A, List[String]) => IntermediateRequestParseResult[B]): IntermediateRequestParseResult[B]

  def ||[A2 >: A](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2]

  def evalEither: Either[IntermediateRequestParseResult.Root[A], ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])]]
  def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])]

  final def evalFinalResult: FinalRequestParseResult[A] =
    evalEither match {
      case Left(IntermediateRequestParseResult.Success(value, Nil)) => FinalRequestParseResult.Success(value)
      case Left(IntermediateRequestParseResult.NotFound)            => FinalRequestParseResult.NotFound
      case Left(IntermediateRequestParseResult.Success(_, _))       => FinalRequestParseResult.NotFound
      case Left(IntermediateRequestParseResult.Error(error))        => FinalRequestParseResult.Error(error)
      case Right(value)                                             =>
        FinalRequestParseResult.Effect {
          value.flatMap {
            case (value, Nil) => ZIO.succeed(value)
            case _            => ZIO.fail(None)
          }
        }
    }

}
object IntermediateRequestParseResult {

  sealed trait Root[+A] extends IntermediateRequestParseResult[A] {

    override def map[B](f: A => B): IntermediateRequestParseResult.Root[B]

    override final def evalEither: Either[Root[A], ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])]] = this.asLeft

  }

  final case class Success[+A](value: A, remainingPath: List[String]) extends Root[A] {
    override def map[B](f: A => B): Success[B] = IntermediateRequestParseResult.Success(f(value), remainingPath)
    override def flatMap[B](f: (A, List[String]) => IntermediateRequestParseResult[B]): IntermediateRequestParseResult[B] = f(value, remainingPath)
    override def ||[A2 >: A](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2] = this
    override def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])] = ZIO.succeed((value, remainingPath))
  }

  sealed trait RootNonSuccess extends Root[Nothing] {
    override final def map[B](f: Nothing => B): IntermediateRequestParseResult.Root[B] = this
    override final def flatMap[B](f: (Nothing, List[String]) => IntermediateRequestParseResult[B]): IntermediateRequestParseResult[B] = this
  }

  case object NotFound extends RootNonSuccess {
    override def ||[A2 >: Nothing](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2] = that
    override def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (Nothing, List[String])] = ZIO.fail(None)
  }

  final case class Error(error: RequestDecodingFailure) extends RootNonSuccess {
    override def ||[A2 >: Nothing](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2] = this
    override def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (Nothing, List[String])] = ZIO.fail(error.some)
  }

  final case class Effect[+A](effect: ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])]) extends IntermediateRequestParseResult[A] {
    override def map[B](f: A => B): IntermediateRequestParseResult[B] = IntermediateRequestParseResult.Mapped(this, f)
    override def flatMap[B](f: (A, List[String]) => IntermediateRequestParseResult[B]): IntermediateRequestParseResult[B] = IntermediateRequestParseResult.FlatMapped(this, f)
    override def ||[A2 >: A](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2] = IntermediateRequestParseResult.Or(this, () => that)
    override def evalEither: Either[Root[A], ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])]] = Right(effect)
    override def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])] = effect
  }

  final case class Mapped[A, B](a: IntermediateRequestParseResult[A], f: A => B) extends IntermediateRequestParseResult[B] {

    override def map[C](f: B => C): IntermediateRequestParseResult[C] = IntermediateRequestParseResult.Mapped(a, v => f(this.f(v)))
    override def flatMap[C](f: (B, List[String]) => IntermediateRequestParseResult[C]): IntermediateRequestParseResult[C] =
      IntermediateRequestParseResult.FlatMapped(a, (v, rest) => f(this.f(v), rest))
    override def ||[A2 >: B](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2] = IntermediateRequestParseResult.Or(this, () => that)

    override def evalEither: Either[Root[B], ZIO[Scope, Option[RequestDecodingFailure], (B, List[String])]] =
      a.evalEither.bimap(_.map(f), _.map { case (v, rest) => (f(v), rest) })

    override def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (B, List[String])] =
      a.evalEffect.map { case (v, rest) => (f(v), rest) }

  }

  final case class FlatMapped[A, B](a: IntermediateRequestParseResult[A], f: (A, List[String]) => IntermediateRequestParseResult[B]) extends IntermediateRequestParseResult[B] {

    override def map[C](f: B => C): IntermediateRequestParseResult[C] = IntermediateRequestParseResult.Mapped(this, f)
    override def flatMap[C](f: (B, List[String]) => IntermediateRequestParseResult[C]): IntermediateRequestParseResult[C] = IntermediateRequestParseResult.FlatMapped(this, f)
    override def ||[A2 >: B](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2] = IntermediateRequestParseResult.Or(this, () => that)

    override def evalEither: Either[Root[B], ZIO[Scope, Option[RequestDecodingFailure], (B, List[String])]] =
      a.evalEither match
        case Left(value)  => value.flatMap(f).evalEither
        case Right(value) => Right(value.flatMap { f(_, _).evalEffect })

    override def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (B, List[String])] =
      a.evalEffect.flatMap { f(_, _).evalEffect }

  }

  final case class Or[A](a: IntermediateRequestParseResult[A], b: () => IntermediateRequestParseResult[A]) extends IntermediateRequestParseResult[A] {

    override def map[B](f: A => B): IntermediateRequestParseResult[B] = IntermediateRequestParseResult.Mapped(this, f)

    override def flatMap[B](f: (A, List[String]) => IntermediateRequestParseResult[B]): IntermediateRequestParseResult[B] = IntermediateRequestParseResult.FlatMapped(this, f)

    override def ||[A2 >: A](that: => IntermediateRequestParseResult[A2]): IntermediateRequestParseResult[A2] =
      IntermediateRequestParseResult.Or(a, () => IntermediateRequestParseResult.Or(b(), () => that))

    override def evalEither: Either[Root[A], ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])]] =
      a.evalEither match {
        case Left(IntermediateRequestParseResult.NotFound)            => b().evalEither
        case Left(success: IntermediateRequestParseResult.Success[A]) => success.asLeft
        case Left(error: IntermediateRequestParseResult.Error)        => error.asLeft
        case Right(value)                                             =>
          Right {
            value.unsome.asSomeError.flatMap {
              case Some(value) => ZIO.succeed(value)
              case None        => b().evalEffect
            }
          }
      }

    override def evalEffect: ZIO[Scope, Option[RequestDecodingFailure], (A, List[String])] =
      a.evalEffect.unsome.asSomeError.flatMap {
        case Some(value) => ZIO.succeed(value)
        case None        => b().evalEffect
      }

  }

}
