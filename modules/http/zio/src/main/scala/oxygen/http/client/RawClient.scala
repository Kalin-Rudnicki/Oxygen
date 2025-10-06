package oxygen.http.client

import zio.*

final case class RawClient(client: zio.http.Client)
object RawClient extends RawClientPlatformSpecific, RawClientPlatformSpecificImpl {

  val requiringConfig: RLayer[zio.http.ZClient.Config, RawClient] =
    rawRequiringConfig.project(RawClient(_))

  val default: TaskLayer[RawClient] =
    ZLayer.succeed { zio.http.ZClient.Config.default } >>> requiringConfig

}
