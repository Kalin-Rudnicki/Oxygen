package oxygen.core.typeclass

import java.time.*
import java.util.TimeZone
import oxygen.core.TypeTag
import oxygen.json.*
import oxygen.predef.test.*
import zio.{Duration as _, *}

object StringCodecSpec extends OxygenSpecDefault {

  private def roundTripTest[A: {StringCodec, TypeTag}](string: String, value: A): TestSpec =
    test(s"${TypeTag[A].prefixObject} : ${value.toString.unesc("")}") {
      assert(StringCodec[A].decoder.decode(string))(isRight(equalTo(value))) &&
      assert(StringCodec[A].encoder.encode(value))(equalTo(string))
    }
  private def roundTripTest[A: TypeTag](note: String)(string: String, value: A)(codec: StringCodec[A]): TestSpec =
    test(s"${TypeTag[A].prefixObject} : ${value.toString.unesc("")} - $note") {
      assert(codec.decoder.decode(string))(isRight(equalTo(value))) &&
      assert(codec.encoder.encode(value))(equalTo(string))
    }

  private def decodeTest[A: {StringDecoder, TypeTag}](string: String, value: A): TestSpec =
    test(s"${TypeTag[A].prefixObject} : ${value.toString.unesc("")}") {
      assert(StringDecoder[A].decode(string))(isRight(equalTo(value)))
    }

  final case class MyClass(`type`: String, value: String) derives JsonCodec

  override def testSpec: TestSpec =
    suite("StringCodecSpec")(
      suite("standard")(
        suite("string")(
          roundTripTest("string", "string"),
        ),
        suite("boolean")(
          roundTripTest("true", true),
          roundTripTest("false", false),
        ),
        suite("int")(
          roundTripTest("25", 25),
          roundTripTest("-25", -25),
          decodeTest("0x25", 0x25), // TODO (KR) : support hex
        ),
        suite("float")(
          roundTripTest("25.47", 25.47f),
          roundTripTest("-25.47", -25.47f),
        ),
        suite("instant")(
          roundTripTest("2020-09-13T12:26:40Z", Instant.ofEpochSecond(1_600_000_000)),
        ),
        suite("offset date time")(
          roundTripTest("2020-01-01T01:02:03Z", OffsetDateTime.of(LocalDate.of(2020, 1, 1), LocalTime.of(1, 2, 3), ZoneOffset.UTC)),
          roundTripTest("2020-01-01T01:02:03+05:00", OffsetDateTime.of(LocalDate.of(2020, 1, 1), LocalTime.of(1, 2, 3), ZoneOffset.ofHours(5))),
        ),
        suite("zoned date time")(
          roundTripTest("2020-01-01T01:02:03Z", ZonedDateTime.of(LocalDate.of(2020, 1, 1), LocalTime.of(1, 2, 3), ZoneOffset.UTC)),
          roundTripTest("2020-01-01T01:02:03+05:00", ZonedDateTime.of(LocalDate.of(2020, 1, 1), LocalTime.of(1, 2, 3), ZoneOffset.ofHours(5))),
          roundTripTest("2020-01-01T01:02:03-07:00[America/Denver]", ZonedDateTime.of(LocalDate.of(2020, 1, 1), LocalTime.of(1, 2, 3), ZoneId.of("America/Denver"))),
        ),
        suite("zone id")(
          roundTripTest("America/Denver", ZoneId.of("America/Denver")),
        ),
        suite("zone offset")(
          roundTripTest("+05:00", ZoneOffset.ofHours(5)),
          roundTripTest("-07:00", ZoneOffset.ofHours(-7)),
        ),
        suite("time zone")(
          roundTripTest("MST", TimeZone.getTimeZone("MST")),
        ),
        suite("local time")(
          roundTripTest("15:30:45", LocalTime.of(15, 30, 45)),
          roundTripTest("03:30:45", LocalTime.of(3, 30, 45)),
          decodeTest("1 AM", LocalTime.of(1, 0)),
          decodeTest("1:23 AM", LocalTime.of(1, 23)),
          decodeTest("1:23:45 AM", LocalTime.of(1, 23, 45)),
          decodeTest("1 PM", LocalTime.of(13, 0)),
          decodeTest("1:23 PM", LocalTime.of(13, 23)),
          decodeTest("1:23:45 PM", LocalTime.of(13, 23, 45)),
          decodeTest("12 AM", LocalTime.of(0, 0)),
          decodeTest("12 PM", LocalTime.of(12, 0)),
        ),
        suite("local date")(
          roundTripTest("2020-02-04", LocalDate.of(2020, 2, 4)),
          decodeTest("2/4/20", LocalDate.of(2020, 2, 4)),
          decodeTest("2/4/2020", LocalDate.of(2020, 2, 4)),
          decodeTest("2/4/1820", LocalDate.of(1820, 2, 4)),
          decodeTest("4.2.20", LocalDate.of(2020, 2, 4)),
          decodeTest("4.2.1820", LocalDate.of(1820, 2, 4)),
        ),
        suite("local date time")(
          roundTripTest("2024-02-04T15:30:45", LocalDateTime.of(2024, 2, 4, 15, 30, 45)),
          decodeTest("2/4/20 2PM", LocalDateTime.of(2020, 2, 4, 14, 0)),
          decodeTest("2/4/20 @ 2:40PM", LocalDateTime.of(2020, 2, 4, 14, 40)),
        ),
        suite("duration")(
          roundTripTest("PT1H", Duration.ofHours(1)),
          decodeTest("1h + 30m", Duration.ofHours(1) + Duration.ofMinutes(30)),
          decodeTest("4 weeks", Duration.ofDays(28)),
        ),
      ),
      suite("base64")(
        roundTripTest("base64")("QUJDCkRFRiU=", "ABC\nDEF%")(StringCodec[String] @@ StringCodec.base64),
        roundTripTest("base64Url")("QUJDCkRFRiU=", "ABC\nDEF%")(StringCodec[String] @@ StringCodec.base64Url),
        roundTripTest("base64Mime")("QUJDCkRFRiU=", "ABC\nDEF%")(StringCodec[String] @@ StringCodec.base64Mime),
        roundTripTest("base64NoPadding")("QUJDCkRFRiU", "ABC\nDEF%")(StringCodec[String] @@ StringCodec.base64NoPadding),
        roundTripTest("base64UrlNoPadding")("QUJDCkRFRiU", "ABC\nDEF%")(StringCodec[String] @@ StringCodec.base64UrlNoPadding),
        roundTripTest("base64MimeNoPadding")("QUJDCkRFRiU", "ABC\nDEF%")(StringCodec[String] @@ StringCodec.base64MimeNoPadding),
      ),
      suite("json")(
        roundTripTest("base64UrlNoPadding")("eyJ0eXBlIjoiYmFzZTY0IiwidmFsdWUiOiJTdHJpbmcifQ", MyClass("base64", "String"))(
          StringCodec.usingJsonCodec[MyClass] @@ StringCodec.base64UrlNoPadding,
        ),
      ),
      suite("misc")(
        roundTripTest("having fun with it (1)")("[[2024-01-02]]", LocalDate.of(2024, 1, 2))(StringCodec.localDate @@ StringCodec.wrappedString("[[", "]]")),
        roundTripTest("having fun with it (2)")("<{MjAyMC0wOS0xM1QxMjoyNjo0MFo}>", Instant.ofEpochSecond(1_600_000_000))(
          StringCodec.instant @@
            StringCodec.base64NoPadding @@
            StringCodec.wrappedString("{", "}") @@
            StringCodec.wrappedString("<", ">"),
        ),
        roundTripTest("having fun with it (3)")("--", "")(StringCodec.string @@ StringCodec.wrappedString("-", "-")),
        test("surround doesn't duplicate") {
          val codec = StringCodec.string @@ StringCodec.wrappedString("-", "-")
          assert(codec.decoder.decode("-"))(isLeft)
        },
      ),
    )

}
