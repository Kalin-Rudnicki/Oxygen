package oxygen.http

import oxygen.http.client.*
import oxygen.http.server.*
import oxygen.predef.test.*
import scala.annotation.experimental

@experimental
object ViaHttpSpec extends OxygenContractSpec[UserApi]("ViaHttpSpec", UserApiContract) {

  // override def defaultLogLevel: LogLevel = LogLevel.Info

  override def layerProvider: LayerProvider[R] =
    LayerProvider.providePerTest[Env](
      ZLayer { Random.RandomLive.nextIntBetween(6000, 7000) },
      Server.layer.simple,
      EndpointAndClientBuilder.layer {
        _.add[UserApi](UserApiImpl.layer)
      },
      CompiledEndpoints.endpointLayer(),
      Client.layer.localPort,
      Server.Config.defaultLayer,
      Server.layer.serving,
    )

}
