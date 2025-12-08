package oxygen.http

import oxygen.predef.test.*
import scala.annotation.experimental

@experimental
object DirectSpec extends OxygenContractSpec[UserApi]("DirectSpec", UserApiContract) {

  // override def defaultLogLevel: LogLevel = LogLevel.Info

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.withLiveClock)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.providePerTest[Env](
      UserApiImpl.layer,
    )

}
