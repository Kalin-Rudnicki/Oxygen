package oxygen.http

import oxygen.http.client.*
import oxygen.http.server.*
import oxygen.predef.test.*
import scala.annotation.experimental

@experimental
object ViaHttpSpec extends OxygenContractSpec[UserApi]("ViaHttpSpec", UserApiContract) {

  // override def defaultLogLevel: LogLevel = LogLevel.Info

  private val serverLayer: RLayer[Int, Unit] =
    ZLayer.makeSome[Int, Unit](
      Server.layer.simple,
      Endpoints.layer(
        UserApiImpl.layer,
      ),
      CompiledEndpoints.endpointLayer(),
      Server.Config.defaultLayer,
      Server.layer.serving,
    )

  private val clientLayer: RLayer[Int, UserApi] =
    ZLayer.makeSome[Int, UserApi](
      Client.layer.localPort,
      DeriveClient.clientLayer[UserApi],
    )

  override def layerProvider: LayerProvider[R] =
    LayerProvider.providePerTest[Env](
      ZLayer { Random.RandomLive.nextIntBetween(6000, 7000) },
      serverLayer,
      clientLayer,
    )

}
