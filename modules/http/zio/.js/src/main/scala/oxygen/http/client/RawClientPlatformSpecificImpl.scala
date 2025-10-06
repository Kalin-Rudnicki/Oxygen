package oxygen.http.client

import zio.*

trait RawClientPlatformSpecificImpl { self: RawClientPlatformSpecific =>

  override protected def rawRequiringConfig: RLayer[zio.http.ZClient.Config, zio.http.Client] =
    zio.http.ZClient.live

}
