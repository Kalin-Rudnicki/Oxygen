package oxygen.http.client

import zio.*
import zio.http.{Request, Response, URL}

final case class ZioHttpClient(
    client: zio.http.Client,
    config: oxygen.http.client.Client.Config,
) extends Client {

  private val modifiedClient: zio.http.Client =
    client.addUrl(zio.http.URL(path = config.path, kind = config.kind))

  override def send(request: SendRequest, extras: Client.RequestExtras): RIO[Scope, Response] = {
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

    val baseEffect: RIO[Scope, Response] =
      for {
        _ <- ZIO.logDebug(s"Sending request [${fullRequest.method}] ${fullRequest.url.encode}")
        response <- modifiedClient.request(fullRequest)
        _ <- ZIO.logDebug(s"Response status: ${response.status}")
      } yield response

    baseEffect @@ ZIOAspect.annotated("api-name" -> extras.apiName, "endpoint-name" -> extras.endpointName)
  }

}
