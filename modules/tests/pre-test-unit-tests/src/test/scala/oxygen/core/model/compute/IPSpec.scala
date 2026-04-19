package oxygen.core.model.compute

import oxygen.predef.test.*
import scala.util.Try
import zio.test.assert

object IPSpec extends OxygenSpecDefault {

  private def codecSpec[A: StringCodec as codec](in: String)(exp: A): TestSpec =
    test(in) {
      assert(codec.decoder.decodeDetailed(in))(isRight(equalTo(exp))) &&
      assertTrue(codec.encoder.encode(exp) == in)
    }
  private def failedCodecSpec[A: StringCodec as codec](in: String): TestSpec =
    test(s"fails : $in") {
      assert(codec.decoder.decodeDetailed(in))(isLeft)
    }

  override def testSpec: TestSpec =
    suite("IPSpec")(
      suite("codec")(
        suite("IPV4")(
          codecSpec[IPV4]("0.0.0.0")(IPV4.of(0, 0, 0, 0)),
          codecSpec[IPV4]("192.168.0.1")(IPV4.of(192, 168, 0, 1)),
          codecSpec[IPV4]("172.24.5.10")(IPV4.of(172, 24, 5, 10)),
          //
          failedCodecSpec[IPV4]("172.24.5.256"),
          failedCodecSpec[IPV4]("172.24.5"),
          failedCodecSpec[IPV4]("172.24.5.255.7"),
        ),
        suite("IPV4CIDR")(
          codecSpec[IPV4CIDR]("0.0.0.0/0")(IPV4CIDR._0(IPV4.of(0, 0, 0, 0))),
          codecSpec[IPV4CIDR]("192.168.0.1/24")(IPV4CIDR._24(IPV4.of(192, 168, 0, 1))),
          codecSpec[IPV4CIDR]("172.24.5.10/16")(IPV4CIDR._16(IPV4.of(172, 24, 5, 10))),
          //
          failedCodecSpec[IPV4CIDR]("172.24.5.256/16"),
          failedCodecSpec[IPV4CIDR]("172.24.5/16"),
          failedCodecSpec[IPV4CIDR]("172.24.5.255.7/16"),
        ),
        suite("IPV4Range")(
          codecSpec[IPV4Range]("0.0.0.0-192.168.0.1")(IPV4Range(IPV4.of(0, 0, 0, 0), IPV4.of(192, 168, 0, 1))),
          codecSpec[IPV4Range]("0.0.0.0-172.24.5.10")(IPV4Range(IPV4.of(0, 0, 0, 0), IPV4.of(172, 24, 5, 10))),
          //
          failedCodecSpec[IPV4Range]("0.0.0.0-172.24.5.256"),
          failedCodecSpec[IPV4Range]("0.0.0.0-172.24.5"),
          failedCodecSpec[IPV4Range]("0.0.0.0-172.24.5.255.7"),
        ),
        suite("NetworkSpec")(
          codecSpec[NetworkSpec]("0.0.0.0")(NetworkSpec.Single(IPV4.of(0, 0, 0, 0))),
          codecSpec[NetworkSpec]("192.168.0.1")(NetworkSpec.Single(IPV4.of(192, 168, 0, 1))),
          codecSpec[NetworkSpec]("172.24.5.10")(NetworkSpec.Single(IPV4.of(172, 24, 5, 10))),
          codecSpec[NetworkSpec]("0.0.0.0/0")(NetworkSpec.CIDR(IPV4CIDR._0(IPV4.of(0, 0, 0, 0)))),
          codecSpec[NetworkSpec]("192.168.0.1/24")(NetworkSpec.CIDR(IPV4CIDR._24(IPV4.of(192, 168, 0, 1)))),
          codecSpec[NetworkSpec]("172.24.5.10/16")(NetworkSpec.CIDR(IPV4CIDR._16(IPV4.of(172, 24, 5, 10)))),
          codecSpec[NetworkSpec]("0.0.0.0-192.168.0.1")(NetworkSpec.Range(IPV4Range(IPV4.of(0, 0, 0, 0), IPV4.of(192, 168, 0, 1)))),
          codecSpec[NetworkSpec]("0.0.0.0-172.24.5.10")(NetworkSpec.Range(IPV4Range(IPV4.of(0, 0, 0, 0), IPV4.of(172, 24, 5, 10)))),
          //
          failedCodecSpec[NetworkSpec]("172.24.5.256"),
          failedCodecSpec[NetworkSpec]("172.24.5"),
          failedCodecSpec[NetworkSpec]("172.24.5.255.7"),
          failedCodecSpec[NetworkSpec]("172.24.5.256/16"),
          failedCodecSpec[NetworkSpec]("172.24.5/16"),
          failedCodecSpec[NetworkSpec]("172.24.5.255.7/16"),
          failedCodecSpec[NetworkSpec]("0.0.0.0-172.24.5.256"),
          failedCodecSpec[NetworkSpec]("0.0.0.0-172.24.5"),
          failedCodecSpec[NetworkSpec]("0.0.0.0-172.24.5.255.7"),
        ),
      ), {
        val subnet1CIDR: IPV4CIDR.BlockStart = IPV4CIDR.BlockStart("172.26.0.0/16")
        val subnet2CIDR: IPV4CIDR.BlockStart = IPV4CIDR.BlockStart("172.27.0.0/16")
        val group1CIDR: IPV4CIDR.BlockStart = IPV4CIDR.BlockStart("172.26.8.0/22")
        val group2CIDR: IPV4CIDR.BlockStart = IPV4CIDR.BlockStart("172.26.9.0/24")
        val group3CIDR: IPV4CIDR.BlockStart = IPV4CIDR.BlockStart("172.26.10.0/24")

        object node1 {
          val subnetIP: IPV4CIDR = IPV4CIDR("172.26.8.1/16")
          val group1IP: IPV4CIDR = IPV4CIDR("172.26.8.1/22")
          val group4IP: IPV4CIDR = IPV4CIDR("172.26.8.1/24")
        }
        object node2 {
          val subnetIP: IPV4CIDR = IPV4CIDR("172.26.9.2/16")
          val group1IP: IPV4CIDR = IPV4CIDR("172.26.9.2/22")
          val group2IP: IPV4CIDR = IPV4CIDR("172.26.9.2/24")
        }
        object node3 {
          val subnetIP: IPV4CIDR = IPV4CIDR("172.26.10.15/16")
          val group1IP: IPV4CIDR = IPV4CIDR("172.26.10.15/22")
          val group3IP: IPV4CIDR = IPV4CIDR("172.26.10.15/24")
        }

        inline def parts(inline name: String, cidr: IPV4CIDR)(inline start: String, inline router: String, inline end: String): TestSpec =
          test(name) {
            assertTrue(
              cidr.blockStartIP == IPV4(start),
              cidr.blockEndIP == IPV4(end),
              cidr.canonicalRouterIP == IPV4(router),
            )
          }

        suite("relationships")(
          suite("parts")(
            parts("subnet1", subnet1CIDR)("172.26.0.0", "172.26.0.1", "172.26.255.255"),
            parts("subnet2", subnet2CIDR)("172.27.0.0", "172.27.0.1", "172.27.255.255"),
            parts("node1.subnetIP", node1.subnetIP)("172.26.0.0", "172.26.0.1", "172.26.255.255"),
            parts("node1.group1IP", node1.group1IP)("172.26.8.0", "172.26.8.1", "172.26.11.255"),
            parts("node2.subnetIP", node2.subnetIP)("172.26.0.0", "172.26.0.1", "172.26.255.255"),
            parts("node2.group1IP", node2.group1IP)("172.26.8.0", "172.26.8.1", "172.26.11.255"),
            parts("node2.group2IP", node2.group2IP)("172.26.9.0", "172.26.9.1", "172.26.9.255"),
            parts("node3.subnetIP", node3.subnetIP)("172.26.0.0", "172.26.0.1", "172.26.255.255"),
            parts("node3.group1IP", node3.group1IP)("172.26.8.0", "172.26.8.1", "172.26.11.255"),
            parts("node3.group3IP", node3.group3IP)("172.26.10.0", "172.26.10.1", "172.26.10.255"),
          ),
          test("contains(IPV4)") {
            assertTrue(
              //
              subnet1CIDR.contains(subnet1CIDR.ip),
              subnet1CIDR.contains(group1CIDR.ip),
              subnet1CIDR.contains(group2CIDR.ip),
              subnet1CIDR.contains(group3CIDR.ip),
              subnet1CIDR.contains(node1.subnetIP.ip),
              subnet1CIDR.contains(node2.subnetIP.ip),
              subnet1CIDR.contains(node3.subnetIP.ip),
              //
              !group1CIDR.contains(subnet1CIDR.ip),
              group1CIDR.contains(group1CIDR.ip),
              group1CIDR.contains(group2CIDR.ip),
              group1CIDR.contains(group3CIDR.ip),
              group1CIDR.contains(node1.subnetIP.ip),
              group1CIDR.contains(node2.subnetIP.ip),
              group1CIDR.contains(node3.subnetIP.ip),
              //
              !group2CIDR.contains(subnet1CIDR.ip),
              !group2CIDR.contains(group1CIDR.ip),
              group2CIDR.contains(group2CIDR.ip),
              !group2CIDR.contains(group3CIDR.ip),
              !group2CIDR.contains(node1.subnetIP.ip),
              group2CIDR.contains(node2.subnetIP.ip),
              !group2CIDR.contains(node3.subnetIP.ip),
              //
              !group3CIDR.contains(subnet1CIDR.ip),
              !group3CIDR.contains(group1CIDR.ip),
              !group3CIDR.contains(group2CIDR.ip),
              group3CIDR.contains(group3CIDR.ip),
              !group3CIDR.contains(node1.subnetIP.ip),
              !group3CIDR.contains(node2.subnetIP.ip),
              group3CIDR.contains(node3.subnetIP.ip),
              //
              !node1.group1IP.contains(subnet1CIDR.ip),
              node1.group1IP.contains(group1CIDR.ip),
              node1.group1IP.contains(group2CIDR.ip),
              node1.group1IP.contains(group3CIDR.ip),
              node1.group1IP.contains(node1.subnetIP.ip),
              node1.group1IP.contains(node2.subnetIP.ip),
              node1.group1IP.contains(node3.subnetIP.ip),
              //
              !node2.group2IP.contains(subnet1CIDR.ip),
              !node2.group2IP.contains(group1CIDR.ip),
              node2.group2IP.contains(group2CIDR.ip),
              !node2.group2IP.contains(group3CIDR.ip),
              !node2.group2IP.contains(node1.subnetIP.ip),
              node2.group2IP.contains(node2.subnetIP.ip),
              !node2.group2IP.contains(node3.subnetIP.ip),
              //
            )
          },
          test("contains(IPV4CIDR)") {
            assertTrue(
              //
              subnet1CIDR.contains(subnet1CIDR),
              subnet1CIDR.contains(group1CIDR),
              subnet1CIDR.contains(group2CIDR),
              subnet1CIDR.contains(group3CIDR),
              //
              !group1CIDR.contains(subnet1CIDR),
              group1CIDR.contains(group1CIDR),
              group1CIDR.contains(node1.group4IP),
              !node1.group4IP.contains(group1CIDR),
              group1CIDR.contains(group2CIDR),
              group1CIDR.contains(group3CIDR),
              //
              !group2CIDR.contains(subnet1CIDR),
              !group2CIDR.contains(group1CIDR),
              group2CIDR.contains(group2CIDR),
              !group2CIDR.contains(group3CIDR),
              //
              !group3CIDR.contains(subnet1CIDR),
              !group3CIDR.contains(group1CIDR),
              !group3CIDR.contains(group2CIDR),
              group3CIDR.contains(group3CIDR),
              //
            )
          },
          test("sameSubnet(IPV4)") {
            assertTrue(
              //
              node1.subnetIP.sameSubnet(node1.subnetIP),
              !node1.subnetIP.sameSubnet(node1.group1IP),
              node1.subnetIP.sameSubnet(node2.subnetIP),
              !node1.subnetIP.sameSubnet(node2.group1IP),
              !node1.subnetIP.sameSubnet(node2.group2IP),
              node1.subnetIP.sameSubnet(node3.subnetIP),
              !node1.subnetIP.sameSubnet(node3.group1IP),
              !node1.subnetIP.sameSubnet(node3.group3IP),
              //
              !node1.group1IP.sameSubnet(node1.subnetIP),
              node1.group1IP.sameSubnet(node1.group1IP),
              !node1.group1IP.sameSubnet(node2.subnetIP),
              node1.group1IP.sameSubnet(node2.group1IP),
              !node1.group1IP.sameSubnet(node2.group2IP),
              !node1.group1IP.sameSubnet(node3.subnetIP),
              node1.group1IP.sameSubnet(node3.group1IP),
              !node1.group1IP.sameSubnet(node3.group3IP),
              //
              !node2.group2IP.sameSubnet(node1.subnetIP),
              !node2.group2IP.sameSubnet(node1.group1IP),
              !node2.group2IP.sameSubnet(node2.subnetIP),
              !node2.group2IP.sameSubnet(node2.group1IP),
              node2.group2IP.sameSubnet(node2.group2IP),
              !node2.group2IP.sameSubnet(node3.subnetIP),
              !node2.group2IP.sameSubnet(node3.group1IP),
              !node2.group2IP.sameSubnet(node3.group3IP),
              //
              !node1.group1IP.sameSubnet(node1.group4IP),
              !node1.group4IP.sameSubnet(node1.group1IP),
              //
            )
          },
          test("sameNetwork(IPV4)") {
            assertTrue(
              //
              node1.subnetIP.sameNetwork(node1.subnetIP),
              !node1.subnetIP.sameNetwork(node1.group1IP),
              node1.subnetIP.sameNetwork(node2.subnetIP),
              !node1.subnetIP.sameNetwork(node2.group1IP),
              !node1.subnetIP.sameNetwork(node2.group2IP),
              node1.subnetIP.sameNetwork(node3.subnetIP),
              !node1.subnetIP.sameNetwork(node3.group1IP),
              !node1.subnetIP.sameNetwork(node3.group3IP),
              //
              !node1.group1IP.sameNetwork(node1.subnetIP),
              node1.group1IP.sameNetwork(node1.group1IP),
              !node1.group1IP.sameNetwork(node2.subnetIP),
              node1.group1IP.sameNetwork(node2.group1IP),
              !node1.group1IP.sameNetwork(node2.group2IP),
              !node1.group1IP.sameNetwork(node3.subnetIP),
              node1.group1IP.sameNetwork(node3.group1IP),
              !node1.group1IP.sameNetwork(node3.group3IP),
              //
              !node2.group2IP.sameNetwork(node1.subnetIP),
              !node2.group2IP.sameNetwork(node1.group1IP),
              !node2.group2IP.sameNetwork(node2.subnetIP),
              !node2.group2IP.sameNetwork(node2.group1IP),
              node2.group2IP.sameNetwork(node2.group2IP),
              !node2.group2IP.sameNetwork(node3.subnetIP),
              !node2.group2IP.sameNetwork(node3.group1IP),
              !node2.group2IP.sameNetwork(node3.group3IP),
              //
              node1.group1IP.sameNetwork(node1.group4IP),
              node1.group4IP.sameNetwork(node1.group1IP),
              //
            )
          },
          test("<>  /  ><") {
            assertTrue(
              //
              subnet1CIDR <> subnet1CIDR,
              subnet1CIDR <> group1CIDR,
              subnet1CIDR <> group2CIDR,
              subnet1CIDR <> group3CIDR,
              //
              group1CIDR <> subnet1CIDR,
              group1CIDR <> group1CIDR,
              group1CIDR <> group2CIDR,
              group1CIDR <> group3CIDR,
              //
              group2CIDR <> subnet1CIDR,
              group2CIDR <> group1CIDR,
              group2CIDR <> group2CIDR,
              group2CIDR >< group3CIDR,
              //
              group3CIDR <> subnet1CIDR,
              group3CIDR <> group1CIDR,
              group3CIDR >< group2CIDR,
              group3CIDR <> group3CIDR,
              //
            )
          },
        )
      },
      suite("addAvoidingTrailing_0_255")(
        test("valid - plus") {
          assertTrue(
            IPV4("172.26.0.1") +? 0 == IPV4("172.26.0.1"),
            IPV4("172.26.0.1") +? 1 == IPV4("172.26.0.2"),
            IPV4("172.26.0.252") +? 2 == IPV4("172.26.0.254"),
            IPV4("172.26.0.252") +? 3 == IPV4("172.26.1.1"),
            IPV4("172.26.0.252") +? 5 == IPV4("172.26.1.3"),
            IPV4("172.26.0.1") +? 508 == IPV4("172.26.2.1"),
            IPV4("172.26.0.11") +? 497 == IPV4("172.26.1.254"),
            IPV4("172.26.0.11") +? 498 == IPV4("172.26.2.1"),
          )
        },
        test("valid - minus") {
          assertTrue(
            IPV4("172.26.0.1") -? 0 == IPV4("172.26.0.1"),
            IPV4("172.26.0.1") -? 1 == IPV4("172.25.255.254"),
            IPV4("172.26.0.1") -? 254 == IPV4("172.25.255.1"),
            IPV4("172.26.0.1") -? 255 == IPV4("172.25.254.254"),
            IPV4("172.26.0.10") -? 256 == IPV4("172.25.255.8"),
          )
        },
        test("invalid") {
          assertTrue(
            Try { IPV4("172.26.0.0") +? 1 }.isFailure,
          )
        },
      ),
    )

}
