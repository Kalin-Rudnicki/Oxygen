package oxygen.http.server

import zio.*
import zio.http.Server as ZServer
import zio.http.netty.NettyConfig

trait Server {

  def serve(
      endpoints: CompiledEndpoints,
      config: Server.Config,
  ): RIO[Scope, Unit]

}
object Server {

  final case class Config(
      errorConfig: ServerErrorConfig,
  )
  object Config {

    val default: Config =
      Config(
        // exposing internal errors in production is bad, but anyone running this in production hopefully isn't using defaults for everything...
        errorConfig = ServerErrorConfig(exposeInternalErrors = true),
      )

    val defaultLayer: ULayer[Server.Config] = ZLayer.succeed(default)

  }

  def serve(
      endpoints: CompiledEndpoints,
      config: Server.Config,
  ): RIO[Server & Scope, Unit] =
    ZIO.serviceWithZIO[Server](_.serve(endpoints, config))

  def serve(
      endpoints: CompiledEndpoints,
  ): RIO[Server & Server.Config & Scope, Unit] =
    ZIO.serviceWithZIO[Server.Config](Server.serve(endpoints, _))

  def serve: RIO[Server & Server.Config & CompiledEndpoints & Scope, Unit] =
    ZIO.serviceWithZIO[CompiledEndpoints](Server.serve(_))

  object layer {

    val customized: URLayer[ZServer.Config & NettyConfig, Server] =
      ZLayer.fromFunction { ZioHttpServer.apply }

    val live: URLayer[ZServer.Config, Server] =
      ZLayer.succeed(NettyConfig.default) >>> customized

    val default: ULayer[Server] =
      ZLayer.succeed(ZServer.Config.default) >>> live

    def simple(port: Int): ULayer[Server] =
      ZLayer.succeed(ZServer.Config.default.port(port)) >>> live

    def simple: URLayer[Int, Server] =
      for {
        port <- ZLayer.service[Int]
        server <- simple(port.get)
      } yield server

    def serving(
        endpoints: CompiledEndpoints,
        config: Server.Config,
    ): RLayer[Server, Unit] =
      ZLayer.scoped { Server.serve(endpoints, config) }

    def serving(
        endpoints: CompiledEndpoints,
    ): RLayer[Server & Server.Config, Unit] =
      ZLayer.scoped { Server.serve(endpoints) }

    def serving: RLayer[Server & Server.Config & CompiledEndpoints, Unit] =
      ZLayer.scoped { Server.serve }

    def serving(
        endpoints: AppliedEndpoints,
        config: Server.Config = Server.Config.default,
        requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
        responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
        endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
    ): RLayer[Server, Unit] =
      (
        CompiledEndpoints.layer(endpoints, requestMiddleware, responseMiddleware, endpointMiddleware) ++
          ZLayer.succeed(config)
      ) >>> serving

  }

}
