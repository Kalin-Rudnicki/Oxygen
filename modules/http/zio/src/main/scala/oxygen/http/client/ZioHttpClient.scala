package oxygen.http.client

import oxygen.zio.syntax.log.*
import zio.*
import zio.http.{Request, Response, URL}

final case class ZioHttpClient(
    rawClient: RawClient,
    logLevel: LogLevel,
) extends Client {

  override lazy val config: Client.Config = Client.Config(rawClient.client.url.kind, rawClient.client.url.path, logLevel)

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
        _ <- ZIO.logAtLevel(logLevel)(s"Sending request [${rawRequest.method}] ${rawRequest.url.encode}", Cause.Empty)
        response <- rawClient.client.request(rawRequest)
        _ <- ZIO.logAtLevel(logLevel)(s"Response status: ${response.status}", Cause.Empty)
      } yield response

    baseEffect @@ ZIOAspect.annotated("api-name" -> extras.apiName, "endpoint-name" -> extras.endpointName)
  }

}
