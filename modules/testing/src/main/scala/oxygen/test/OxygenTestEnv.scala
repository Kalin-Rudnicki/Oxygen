package oxygen.test

import zio.*

// TODO (KR) : LogCache
type OxygenTestEnv = Any
object OxygenTestEnv {

  val layer: ULayer[OxygenTestEnv] =
    ZLayer.make[OxygenTestEnv](
      // TODO (KR) :
      // LogCache.layer,
    )

}
