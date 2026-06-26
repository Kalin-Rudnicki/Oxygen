package oxygen.crypto.model

import oxygen.json.*
import oxygen.predef.test.*

object OidcUserInfoSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("OidcUserInfoSpec")(
      test("parses standard claims and exposes non-standard ones (e.g. groups) via raw") {
        val json = Json.obj(
          "sub" -> Json.string("user-abc"),
          "email" -> Json.string("ada@example.com"),
          "email_verified" -> Json.boolean(true),
          "name" -> Json.string("Ada Lovelace"),
          "preferred_username" -> Json.string("ada"),
          "groups" -> Json.arr(Json.string("admin"), Json.string("eng")),
        )
        val parsed = OidcUserInfo.fromJson(json)
        assertTrue(
          parsed.map(_.sub) == Right("user-abc"),
          parsed.toOption.flatMap(_.email).contains("ada@example.com"),
          parsed.toOption.flatMap(_.emailVerified).contains(true),
          parsed.toOption.flatMap(_.name).contains("Ada Lovelace"),
          parsed.toOption.flatMap(_.preferredUsername).contains("ada"),
          parsed.toOption.map(_.stringArrayClaim("groups")).contains(List("admin", "eng")),
        )
      },
      test("fails when the required `sub` claim is missing") {
        assertTrue(OidcUserInfo.fromJson(Json.obj("email" -> Json.string("x@y.com"))).isLeft)
      },
      test("JsonCodec round-trips (encode preserves the raw response)") {
        val codec = summon[JsonCodec[OidcUserInfo]]
        val info = OidcUserInfo.fromJson(Json.obj("sub" -> Json.string("s"), "email" -> Json.string("e@x.com"))).toOption.get
        val encoded = codec.encoder.encodeJsonStringCompact(info)
        assertTrue(codec.decoder.decodeJsonString(encoded).toOption.map(_.sub).contains("s"))
      },
    )

}
