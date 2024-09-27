package oxygen.test

import zio.*

// TODO (KR) : LogCache
type OxygenTestEnv = Any
object HarnessTestEnv {

  val layer: ULayer[OxygenTestEnv] =
    ZLayer.make[OxygenTestEnv](
      // TODO (KR) :
      // LogCache.layer,
    )

}
