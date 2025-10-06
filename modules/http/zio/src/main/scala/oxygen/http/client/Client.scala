package oxygen.http.client

import zio.*
import zio.http.*

trait Client {

  val config: Client.Config

  def send(request: SendRequest, extras: Client.RequestExtras): RIO[Scope, Response]

}
object Client {

  // TODO (KR) : accept middlewares when creating a client
  //           : ssl

  final case class Config(
      kind: URL.Location,
      path: Path,
  )
  object Config {

    val relativeUrl: Config =
      Config(URL.Location.Relative, Path.empty)

    def layer(urlString: String): TaskLayer[Config] =
      ZLayer {
        for {
          url <- ZIO.fromEither { URL.decode(urlString) }
          _ <- ZIO.fail(new RuntimeException("client config can not have query params")).whenDiscard(url.queryParams.nonEmpty)
          _ <- ZIO.fail(new RuntimeException("client config can not have fragment")).whenDiscard(url.fragment.nonEmpty)
        } yield Config(url.kind, url.path)
      }

  }

  final case class RequestExtras(
      apiName: String,
      endpointName: String,
  )

  object layer {

    val live: URLayer[RawClient & oxygen.http.client.Client.Config, oxygen.http.client.Client] =
      ZLayer.fromFunction { ZioHttpClient(_, _) }.fresh // I hate this `.fresh` so much, but ZLayer is being stupid: https://github.com/zio/zio/issues/10185

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
