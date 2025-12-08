package oxygen.http.client

import oxygen.http.core.{HttpDecodingFailure, RequestDecodingFailure, ResponseDecodingFailure}
import oxygen.predef.core.*
import scala.reflect.TypeTest
import zio.*

trait ClientErrorHandler[E] extends Cause.Folder[Any, Throwable, Cause[E]] {

  def wrapDeath(error: Throwable, trace: StackTrace): Option[E]
  def wrapDecodingFailure(error: ResponseDecodingFailure): Option[E]

  override final def empty(context: Any): Cause[E] = Cause.Empty

  private def throwableCase(t: Throwable, stackTrace: StackTrace): Cause[E] = {
    def fromDecodingFailure: Option[Cause.Fail[E]] = t match
      case t: ResponseDecodingFailure => wrapDecodingFailure(t).map(Cause.Fail(_, stackTrace))
      case _                          => None
    def fromDeath: Option[Cause.Fail[E]] =
      wrapDeath(t, stackTrace).map(Cause.Fail(_, stackTrace))

    fromDecodingFailure.orElse(fromDeath).getOrElse(Cause.Die(t, stackTrace))
  }

  override final def failCase(context: Any, error: Throwable, stackTrace: StackTrace): Cause[E] = throwableCase(error, stackTrace)

  override final def dieCase(context: Any, t: Throwable, stackTrace: StackTrace): Cause[E] = throwableCase(t, stackTrace)

  override final def interruptCase(context: Any, fiberId: FiberId, stackTrace: StackTrace): Cause[E] = Cause.Interrupt(fiberId, stackTrace)

  override final def bothCase(context: Any, left: Cause[E], right: Cause[E]): Cause[E] = Cause.Both(left, right)

  override final def thenCase(context: Any, left: Cause[E], right: Cause[E]): Cause[E] = Cause.Then(left, right)

  override final def stacklessCase(context: Any, value: Cause[E], stackless: Boolean): Cause[E] = Cause.Stackless(value, stackless)

}
object ClientErrorHandler {

  final class NotHandled[E] extends ClientErrorHandler[E] {
    override def wrapDeath(error: Throwable, trace: StackTrace): Option[Nothing] = None
    override def wrapDecodingFailure(error: ResponseDecodingFailure): Option[Nothing] = None
  }

  final class ThrowableSubtype[E](using tt: TypeTest[Throwable, E]) extends ClientErrorHandler[E] {
    override def wrapDeath(error: Throwable, trace: StackTrace): Option[E] = tt.unapply(error)
    override def wrapDecodingFailure(error: ResponseDecodingFailure): Option[E] = tt.unapply(error)
  }

  def notHandled[E]: ClientErrorHandler[E] = new NotHandled[E]
  def throwableSubtype[E <: Throwable](using TypeTest[Throwable, E]): ClientErrorHandler[E] = new ThrowableSubtype[E]

  given throwable: ClientErrorHandler[Throwable] = throwableSubtype
  given httpDecodingFailure: ClientErrorHandler[HttpDecodingFailure] = throwableSubtype
  given requestDecodingFailure: ClientErrorHandler[RequestDecodingFailure] = throwableSubtype
  given responseDecodingFailure: ClientErrorHandler[ResponseDecodingFailure] = throwableSubtype

  given string: ClientErrorHandler[String] =
    new ClientErrorHandler[String] {
      override def wrapDeath(error: Throwable, trace: StackTrace): Option[String] = None
      override def wrapDecodingFailure(error: ResponseDecodingFailure): Option[String] = error.safeGetMessage.some
    }

  given nothing: ClientErrorHandler[Nothing] = notHandled

}
