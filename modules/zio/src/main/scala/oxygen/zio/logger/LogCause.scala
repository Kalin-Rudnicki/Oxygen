package oxygen.zio.logger

import oxygen.predef.json.*
import oxygen.zio.typeclass.ErrorLogger
import zio.*

sealed trait LogCause derives JsonCodec {

  final def toZio: zio.Cause[Json] = this match
    case LogCause.Empty                           => zio.Cause.Empty
    case LogCause.Fail(value, Some(trace))        => zio.Cause.Fail(value, trace)
    case LogCause.Die(value, Some(trace))         => zio.Cause.Die(value, trace)
    case LogCause.Interrupt(fiberId, Some(trace)) => zio.Cause.Interrupt(fiberId, trace)
    case LogCause.Then(left, right)               => zio.Cause.Then(left.toZio, right.toZio)
    case LogCause.Both(left, right)               => zio.Cause.Both(left.toZio, right.toZio)
    case LogCause.Fail(value, None)               => zio.Cause.stackless(zio.Cause.Fail(value, StackTrace.none))
    case LogCause.Die(value, None)                => zio.Cause.stackless(zio.Cause.Die(value, StackTrace.none))
    case LogCause.Interrupt(fiberId, None)        => zio.Cause.stackless(zio.Cause.Interrupt(fiberId, StackTrace.none))

  final def toBases: Chunk[LogCause.Base] = this match
    case LogCause.Empty             => Chunk.empty
    case base: LogCause.Base        => Chunk.single(base)
    case LogCause.Then(left, right) => left.toBases ++ right.toBases
    case LogCause.Both(left, right) => left.toBases ++ right.toBases

}
object LogCause {

  sealed trait Base extends LogCause

  case object Empty extends LogCause
  final case class Fail(value: Json, trace: Option[StackTrace]) extends LogCause.Base
  final case class Die(value: EncodedThrowable, trace: Option[StackTrace]) extends LogCause.Base
  final case class Interrupt(fiberId: FiberId, trace: Option[StackTrace]) extends LogCause.Base
  final case class Then(left: LogCause, right: LogCause) extends LogCause
  final case class Both(left: LogCause, right: LogCause) extends LogCause

  private def fromZio(cause: zio.Cause[Json], stack: Boolean): LogCause = cause match
    case zio.Cause.Empty                       => LogCause.Empty
    case zio.Cause.Fail(value, trace)          => LogCause.Fail(value, Option.when(stack)(trace))
    case zio.Cause.Die(value, trace)           => LogCause.Die(EncodedThrowable.fromThrowable(value), Option.when(stack)(trace))
    case zio.Cause.Interrupt(fiberId, trace)   => LogCause.Interrupt(fiberId, Option.when(stack)(trace))
    case zio.Cause.Stackless(cause, stackless) => LogCause.fromZio(cause, !stackless)
    case zio.Cause.Then(left, right)           => LogCause.Then(LogCause.fromZio(left, stack), LogCause.fromZio(right, stack))
    case zio.Cause.Both(left, right)           => LogCause.Both(LogCause.fromZio(left, stack), LogCause.fromZio(right, stack))

  def fromZio(cause: zio.Cause[Json]): LogCause = LogCause.fromZio(cause, true)
  def fromZio[E](cause: zio.Cause[E], errorLogger: ErrorLogger[E]): LogCause = LogCause.fromZio(cause.map(errorLogger.show), true)

}
