package oxygen.http.client

import oxygen.http.core.ResponseDecodingFailure
import zio.*

trait ClientErrorHandler[A] extends Cause.Folder[Any, Nothing, Cause[A]] {

  def wrapDeath(error: Throwable, trace: StackTrace): Option[A]
  def wrapDecodingFailure(error: ResponseDecodingFailure): Option[A]

  override def empty(context: Any): Cause[A] = Cause.Empty

  override def failCase(context: Any, error: Nothing, stackTrace: StackTrace): Cause[A] = Cause.Empty

  override def dieCase(context: Any, t: Throwable, stackTrace: StackTrace): Cause[A] = wrapDeath(t, stackTrace) match
    case Some(typedError) => Cause.Fail(typedError, stackTrace)
    case None             => Cause.Die(t, stackTrace)

  override def interruptCase(context: Any, fiberId: FiberId, stackTrace: StackTrace): Cause[A] = Cause.Interrupt(fiberId, stackTrace)

  override def bothCase(context: Any, left: Cause[A], right: Cause[A]): Cause[A] = Cause.Both(left, right)

  override def thenCase(context: Any, left: Cause[A], right: Cause[A]): Cause[A] = Cause.Then(left, right)

  override def stacklessCase(context: Any, value: Cause[A], stackless: Boolean): Cause[A] = Cause.Stackless(value, stackless)

}
object ClientErrorHandler {

  final class NotHandled[A] extends ClientErrorHandler[A] {
    override def wrapDeath(error: Throwable, trace: StackTrace): Option[Nothing] = None
    override def wrapDecodingFailure(error: ResponseDecodingFailure): Option[Nothing] = None
  }

  def notHandled[A]: ClientErrorHandler[A] = new NotHandled[A]

  given nothing: ClientErrorHandler[Nothing] = notHandled
  given throwable: ClientErrorHandler[Throwable] = notHandled
  given string: ClientErrorHandler[String] = notHandled

}
