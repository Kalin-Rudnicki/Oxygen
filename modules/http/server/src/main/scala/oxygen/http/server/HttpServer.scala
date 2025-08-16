package oxygen.http.server

import zio.*

trait HttpServer {

  /**
    * Calling this should not block until the server is closed.
    * Server should stop when scope is closed.
    */
  def start(config: HttpServer.Config, endpoints: Endpoints, middlewares: Seq[EndpointMiddleware]): RIO[Scope, Unit]

}
object HttpServer extends HttpServerPlatformSpecific, HttpServerPlatformSpecificImpl {

  final case class Config(
      port: Int,
      exposeInternalErrors: Boolean,
  )

  val defaultLayer: ULayer[HttpServer] =
    ZLayer.succeed { defaultServer }

  def runningServerLayer(middlewares: Seq[EndpointMiddleware]): RLayer[Config & Endpoints & HttpServer, Unit] =
    ZLayer.scoped {
      for {
        config <- ZIO.service[Config]
        endpoints <- ZIO.service[Endpoints]
        server <- ZIO.service[HttpServer]
        _ <- server.start(config, endpoints, middlewares)
      } yield ()
    }

  def runningServerLayer: RLayer[Config & Endpoints & HttpServer, Unit] = runningServerLayer(Nil)

}
