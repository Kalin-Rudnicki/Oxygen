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
      HttpServer.defaultLayer,
      ZLayer.service[Int].project { port => HttpServer.Config(port, true) },
      Endpoints.layer(
        UserApiImpl.layer,
      ),
      HttpServer.runningServerLayer,
    )

  private val clientLayer: RLayer[Int, UserApi] =
    ZLayer.makeSome[Int, UserApi](
      ZLayer.service[Int].project { port => JvmHttpClient(ConnectionTarget("localhost", ConnectionTarget.SslConfig.NoSsl, port)) },
      DeriveClient.clientLayer[UserApi],
    )

  override def layerProvider: LayerProvider[R] =
    LayerProvider.providePerTest[Env](
      ZLayer { Random.RandomLive.nextIntBetween(6000, 7000) },
      serverLayer,
      clientLayer,
    )

}
