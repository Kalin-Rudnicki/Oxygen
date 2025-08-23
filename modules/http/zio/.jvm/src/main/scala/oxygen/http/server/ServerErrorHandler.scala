package oxygen.http.server

import oxygen.http.core.RequestDecodingFailure

trait ServerErrorHandler[A] {
  def wrapDeath(error: Throwable, exposeInternalErrors: Boolean): Option[A]
  def wrapDecodingFailure(error: RequestDecodingFailure): Option[A]
}
object ServerErrorHandler {

  final class NotHandled[A] extends ServerErrorHandler[A] {
    override def wrapDeath(error: Throwable, exposeInternalErrors: Boolean): Option[Nothing] = None
    override def wrapDecodingFailure(error: RequestDecodingFailure): Option[Nothing] = None
  }

  given nothing: ServerErrorHandler[Nothing] = new NotHandled[Nothing]
  given throwable: ServerErrorHandler[Throwable] = new NotHandled[Throwable]
  given string: ServerErrorHandler[String] = new NotHandled[String]

}
