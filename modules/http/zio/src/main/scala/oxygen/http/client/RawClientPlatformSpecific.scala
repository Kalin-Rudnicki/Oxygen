package oxygen.http.client

import zio.*

trait RawClientPlatformSpecific {

  protected def rawRequiringConfig: RLayer[zio.http.ZClient.Config, zio.http.Client]

}
