package oxygen.http.client

import oxygen.predef.core.*

final case class HttpClientError(error: Throwable) extends Throwable {
  override def getMessage: String = s"Error sending HTTP request [${error.getClass.getName}] : ${error.safeGetMessage}"
}
