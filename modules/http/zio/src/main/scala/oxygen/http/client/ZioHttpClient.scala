package oxygen.http.client

import zio.*
import zio.http.{Request, Response, URL}

final case class ZioHttpClient(
    rawClient: RawClient,
) extends Client {

  override def config: Client.Config = Client.Config(rawClient.client.url.kind, rawClient.client.url.path)

  override def send(request: SendRequest, extras: Client.RequestExtras): RIO[Scope, Response] = {
    val baseEffect: RIO[Scope, Response] =
      for {
        rawRequest <- ZIO.succeed {
          Request(
            method = request.method,
            url = URL(path = request.path.addLeadingSlash, queryParams = request.queryParams),
            headers = request.headers,
            body = request.body,
          )
        }
        _ <- ZIO.logDebug(s"Sending request [${rawRequest.method}] ${rawRequest.url.encode}")
        response <- rawClient.client.request(rawRequest)
        _ <- ZIO.logDebug(s"Response status: ${response.status}")
      } yield response

    baseEffect @@ ZIOAspect.annotated("api-name" -> extras.apiName, "endpoint-name" -> extras.endpointName)
  }

}
