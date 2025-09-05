package oxygen.http.server

import oxygen.http.core.RequestDecodingFailure
import zio.StackTrace

trait ServerErrorHandler[A] {
  def wrapDeath(error: Throwable, trace: StackTrace, exposeInternalErrors: Boolean): Option[A]
  def wrapDecodingFailure(error: RequestDecodingFailure): Option[A]
}
object ServerErrorHandler {

  final class NotHandled[A] extends ServerErrorHandler[A] {
    override def wrapDeath(error: Throwable, trace: StackTrace, exposeInternalErrors: Boolean): Option[Nothing] = None
    override def wrapDecodingFailure(error: RequestDecodingFailure): Option[Nothing] = None
  }

  def notHandled[A]: ServerErrorHandler[A] = new NotHandled[A]

  given nothing: ServerErrorHandler[Nothing] = notHandled
  given throwable: ServerErrorHandler[Throwable] = notHandled
  given string: ServerErrorHandler[String] = notHandled

}
