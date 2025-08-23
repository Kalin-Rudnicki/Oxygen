package oxygen.http.client

import zio.*
import zio.http.{Request, Response, URL}

final case class ZioHttpClient(
    client: zio.http.Client,
    config: oxygen.http.client.Client.Config,
) extends Client {

  private val modifiedClient: zio.http.Client =
    client.addUrl(zio.http.URL(path = config.path, kind = config.kind))

  override def send(request: SendRequest): RIO[Scope, Response] = {
    val fullRequest: Request =
      Request(
        version = client.version,
        method = request.method,
        url = URL(path = request.path, queryParams = request.queryParams),
        headers = request.headers,
        body = request.body,
        remoteAddress = None,
        remoteCertificate = None,
      )

    modifiedClient.request(fullRequest)
  }

}
