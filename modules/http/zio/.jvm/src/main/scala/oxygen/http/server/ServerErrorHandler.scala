package oxygen.http.server

import oxygen.http.core.{HttpDecodingFailure, RequestDecodingFailure}
import oxygen.zio.ExtractedCauses

trait ServerErrorHandler[A] {
  def convertCause(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[A]
  // TODO (KR) :   def join(a: A, b: A): Option[A]
  //           : or
  //           :   val reduce: Option[(A, A) => A]
}
object ServerErrorHandler {

  final class NotHandled[A] extends ServerErrorHandler[A] {
    override def convertCause(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[A] = None
  }

  def notHandled[A]: ServerErrorHandler[A] = new NotHandled[A]

  given httpDecodingFailure: ServerErrorHandler[HttpDecodingFailure] =
    new ServerErrorHandler[HttpDecodingFailure] {
      override def convertCause(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[HttpDecodingFailure] =
        cause.eitherFailure.toOption
    }

  given requestDecodingFailure: ServerErrorHandler[RequestDecodingFailure] =
    new ServerErrorHandler[RequestDecodingFailure] {
      override def convertCause(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[RequestDecodingFailure] =
        cause.eitherFailure.toOption
    }

  // TODO (KR) : have encoding defaults for these in [[ServerErrorConfig]] too?
  //           : ex: `given String: ...` -> _.defaultStringEncoding match { case ... => throwable.safeGetMessage.some ; case ... => None }

  given throwable: ServerErrorHandler[Throwable] = notHandled

  given string: ServerErrorHandler[String] = notHandled

  given nothing: ServerErrorHandler[Nothing] = notHandled

}
