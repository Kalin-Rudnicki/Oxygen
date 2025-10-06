package oxygen.http.client

import oxygen.predef.test.*
import zio.http.{Scheme, URL}

object MultiClientSpec extends OxygenSpecDefault {

  final case class ClientA(client: Client)
  object ClientA {

    val layer: RLayer[RawClient, ClientA] =
      Client.Config.layer("https://service-a.com") >>> Client.layer.live.project(ClientA(_))

  }

  final case class ClientB(client: Client)
  object ClientB {

    val layer: RLayer[RawClient, ClientB] =
      Client.Config.layer("https://service-b.com") >>> Client.layer.live.project(ClientB(_))

  }

  override def testSpec: TestSpec =
    suite("MultiClientSpec")(
      test("can support multiple clients which send to base URL") {
        for {
          a <- ZIO.service[ClientA]
          b <- ZIO.service[ClientB]
        } yield assert(a.client.config.kind)(equalTo(URL.Location.Absolute(Scheme.HTTPS, "service-a.com", None))).label("a-client") &&
          assert(b.client.config.kind)(equalTo(URL.Location.Absolute(Scheme.HTTPS, "service-b.com", None))).label("b-client")
      }.provide(
        RawClient.default,
        ClientA.layer,
        ClientB.layer,
      ),
    )

}
