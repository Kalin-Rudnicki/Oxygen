package oxygen.http.server

import zio.*
import zio.http.Server as ZServer
import zio.http.netty.NettyConfig

final case class ZioHttpServer(
    zioConfig: ZServer.Config,
    nettyConfig: NettyConfig,
) extends Server {

  private val layer: TaskLayer[ZServer] =
    ZLayer.make[ZServer](
      ZLayer.succeed(zioConfig),
      ZLayer.succeed(nettyConfig),
      ZServer.customized,
    )

  private def serveInternal(endpoints: CompiledEndpoints, config: Server.Config): RIO[ZServer & Scope, Unit] =
    for {
      server <- ZIO.service[ZServer]
      _ <- ZIO.logInfo("starting...")
      _ <- server.install[Any](endpoints.toRoutes(config))
      p <- server.port
      _ <- ZIO.logInfo(s"started: $p")
    } yield ()

  override def serve(endpoints: CompiledEndpoints, config: Server.Config): RIO[Scope, Unit] =
    serveInternal(endpoints, config).provideSome[Scope](layer.extendScope)

}
