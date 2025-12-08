package oxygen.http.model.internal

import java.net.InetAddress
import java.security.cert.Certificate
import oxygen.http.core.ReadOnlyCachedHttpBody
import zio.*
import zio.http.*

final case class ReceivedRequest(
    version: Version,
    method: Method,
    url: URL,
    headers: Headers,
    body: ReadOnlyCachedHttpBody,
    remoteAddress: Option[InetAddress],
    remoteCertificate: Option[Certificate],
) {

  val queryParams: QueryParams = url.queryParams

  val fullPath: List[String] = url.path.removeDotSegments.segments.toList

}
object ReceivedRequest {

  def fromRequest(request: Request): UIO[ReceivedRequest] =
    ReadOnlyCachedHttpBody.wrap(request.body).map { body =>
      ReceivedRequest(
        version = request.version,
        method = request.method,
        url = request.url,
        headers = request.headers,
        body = body,
        remoteAddress = request.remoteAddress,
        remoteCertificate = request.remoteCertificate,
      )
    }

}
