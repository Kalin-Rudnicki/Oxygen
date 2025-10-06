package oxygen.http.client

import zio.*
import zio.http.DnsResolver
import zio.http.netty.NettyConfig

trait RawClientPlatformSpecificImpl { self: RawClientPlatformSpecific =>

  override protected def rawRequiringConfig: RLayer[zio.http.ZClient.Config, zio.http.Client] = {
    given Trace = Trace.empty
    (ZLayer.succeed(NettyConfig.defaultWithFastShutdown) ++ DnsResolver.default) >>> zio.http.ZClient.live
  }

}
