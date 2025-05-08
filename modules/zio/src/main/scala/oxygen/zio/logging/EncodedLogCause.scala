package oxygen.zio.logging

import oxygen.predef.core.*
import oxygen.predef.json.{JsonCodec, given}
import oxygen.zio.instances.given
import zio.{Cause, FiberId, StackTrace}

sealed trait EncodedLogCause derives JsonCodec {

  protected def toSimpleBuilder: Growable[EncodedLogCause.Simple]

  final def toSimple: Contiguous[EncodedLogCause.Simple] =
    toSimpleBuilder.toContiguous

}
object EncodedLogCause {

  sealed trait Simple extends EncodedLogCause {
    val trace: StackTrace
    override protected final def toSimpleBuilder: Growable[Simple] = Growable.single(this)
  }

  final case class Fail(message: String, trace: StackTrace) extends EncodedLogCause.Simple
  final case class Die(error: Throwable, trace: StackTrace) extends EncodedLogCause.Simple
  final case class Interrupt(fiberId: FiberId, trace: StackTrace) extends EncodedLogCause.Simple

  sealed trait Combinator extends EncodedLogCause {
    val left: EncodedLogCause
    val right: EncodedLogCause
    override protected final def toSimpleBuilder: Growable[Simple] = left.toSimpleBuilder ++ right.toSimpleBuilder
  }

  final case class Then(left: EncodedLogCause, right: EncodedLogCause) extends EncodedLogCause.Combinator
  final case class Both(left: EncodedLogCause, right: EncodedLogCause) extends EncodedLogCause.Combinator

  private def rec[A](cause: Cause[A], show: A => String, stack: Boolean): Option[EncodedLogCause] =
    cause match {
      case Cause.Empty                       => None
      case Cause.Fail(value, trace)          => EncodedLogCause.Fail(show(value), if (stack) trace else StackTrace.none).some
      case Cause.Die(value, trace)           => EncodedLogCause.Die(value, if (stack) trace else StackTrace.none).some
      case Cause.Interrupt(fiberId, trace)   => EncodedLogCause.Interrupt(fiberId, if (stack) trace else StackTrace.none).some
      case Cause.Stackless(cause, stackless) => rec(cause, show, stack && !stackless)
      case Cause.Then(left, right) =>
        (rec(left, show, stack), rec(right, show, stack)) match {
          case (Some(left), Some(right)) => EncodedLogCause.Then(left, right).some
          case (Some(left), None)        => left.some
          case (None, Some(right))       => right.some
          case (None, None)              => None
        }
      case Cause.Both(left, right) =>
        (rec(left, show, stack), rec(right, show, stack)) match {
          case (Some(left), Some(right)) => EncodedLogCause.Both(left, right).some
          case (Some(left), None)        => left.some
          case (None, Some(right))       => right.some
          case (None, None)              => None
        }
    }

  def fromCause[A](cause: Cause[A], show: A => String): Option[EncodedLogCause] =
    rec(cause, show, true)

  def fromAnyCause(cause: Cause[Any]): Option[EncodedLogCause] =
    fromCause(cause, String.valueOf(_))

}
