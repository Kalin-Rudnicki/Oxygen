package oxygen.http.client

import oxygen.http.model.*
import zio.*

trait HttpClient {
  val target: ConnectionTarget
  def send(request: HttpRequest): ZIO[Scope, HttpClientError, HttpResponse]
}
object HttpClient extends HttpClientPlatformSpecific, HttpClientPlatformSpecificImpl {

  val defaultLayer: URLayer[ConnectionTarget, HttpClient] =
    ZLayer.fromFunction { defaultClient }

}
