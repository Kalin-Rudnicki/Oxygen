package oxygen.http.server

import oxygen.http.model.DecodingFailure

trait ServerErrorHandler[+A] {
  def wrapDeath(error: Throwable, exposeInternalErrors: Boolean): Option[A]
  def wrapDecodingFailure(error: DecodingFailure): Option[A]
}
object ServerErrorHandler {

  case object NotHandled extends ServerErrorHandler[Nothing] {
    override def wrapDeath(error: Throwable, exposeInternalErrors: Boolean): Option[Nothing] = None
    override def wrapDecodingFailure(error: DecodingFailure): Option[Nothing] = None
  }

  given nothing: ServerErrorHandler[Nothing] = NotHandled
  given throwable: ServerErrorHandler[Throwable] = NotHandled
  given string: ServerErrorHandler[String] = NotHandled

}
