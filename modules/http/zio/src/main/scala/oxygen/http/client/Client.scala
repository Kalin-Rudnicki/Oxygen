package oxygen.http.client

import zio.*
import zio.http.*

trait Client {

  def config: Client.Config

  def send(request: SendRequest, extras: Client.RequestExtras): RIO[Scope, Response]

}
object Client {

  // TODO (KR) : accept middlewares when creating a client
  //           : ssl

  final case class Config(
      kind: URL.Location,
      path: Path,
      logLevel: LogLevel,
  ) {

    def >>(client: RawClient): RawClient =
      RawClient(client.client.url(URL(kind = kind, path = path)))

  }
  object Config {

    def relativeUrl: Config =
      Config(URL.Location.Relative, Path.empty, LogLevel.Info)
    def relativeUrl(logLevel: LogLevel): Config =
      Config(URL.Location.Relative, Path.empty, logLevel)

    def layer(urlString: String, logLevel: LogLevel = LogLevel.Info): TaskLayer[Config] =
      ZLayer {
        for {
          url <- ZIO.fromEither { URL.decode(urlString) }
          _ <- ZIO.fail(new RuntimeException("client config can not have query params")).whenDiscard(url.queryParams.nonEmpty)
          _ <- ZIO.fail(new RuntimeException("client config can not have fragment")).whenDiscard(url.fragment.nonEmpty)
        } yield Config(url.kind, url.path, logLevel)
      }

    def fromClient(client: RawClient, logLevel: LogLevel = LogLevel.Info): Config =
      Config(client.client.url.kind, client.client.url.path, logLevel)

  }

  final case class RequestExtras(
      apiName: String,
      endpointName: String,
  )

  object layer {

    // I hate this `.fresh` so much, but ZLayer is being stupid: https://github.com/zio/zio/issues/10185
    val live: URLayer[RawClient & oxygen.http.client.Client.Config, oxygen.http.client.Client] =
      ZLayer.fromFunction { (client: RawClient, cfg: oxygen.http.client.Client.Config) => ZioHttpClient(cfg >> client, cfg.logLevel) }.fresh

    val raw: URLayer[RawClient, oxygen.http.client.Client] =
      ZLayer.fromFunction { ZioHttpClient(_, LogLevel.Info) }.fresh

    val default: RLayer[oxygen.http.client.Client.Config, oxygen.http.client.Client] =
      RawClient.default >>> live

    def localPort(port: Int): TaskLayer[Client] =
      Config.layer(s"http://localhost:$port") >>> default

    val localPort: RLayer[Int, Client] =
      for {
        port <- ZLayer.service[Int]
        client <- localPort(port.get)
      } yield client

  }

}
