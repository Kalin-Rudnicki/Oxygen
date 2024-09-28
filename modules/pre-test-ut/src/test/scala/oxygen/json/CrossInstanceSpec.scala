package oxygen.json

import oxygen.core.syntax.stringCodec.*
import oxygen.predef.json.*
import oxygen.predef.test.*

object CrossInstanceSpec extends OxygenSpecDefault {

  private def roundTripJsonTest[A: TypeTag](string: String, value: A)(jsonCodec: JsonCodec[A])(implicit loc: SourceLocation): TestSpec =
    test(s"${TypeTag[A].prefixObject} / $string / $value") {
      assert(string.fromJson[A](using jsonCodec.decoder))(isRight(equalTo_unfilteredDiff(value))) &&
      assert(value.toJson(using jsonCodec.encoder))(equalTo_unfilteredDiff(string))
    }

  private def roundTripStringTest[A: TypeTag](string: String, value: A)(stringCodec: StringCodec[A])(implicit loc: SourceLocation): TestSpec =
    test(s"${TypeTag[A].prefixObject} / $string / $value") {
      assert(string.fromString[A](using stringCodec.decoder))(isRight(equalTo_unfilteredDiff(value))) &&
      assert(value.encodeToString(using stringCodec.encoder))(equalTo_unfilteredDiff(string))
    }

  final case class Example1(
      a: Int,
      b: Option[String],
  ) derives JsonCodec

  override def testSpec: TestSpec =
    suite("CrossInstanceSpec")(
      suite("roundTripJson")(
        roundTripJsonTest("\"hello\"", "hello")(StringCodec[String].toJsonCodec),
        roundTripJsonTest("\"123\"", 123)(StringCodec[Int].toJsonCodec),
        roundTripJsonTest("\"[123]\"", 123)((StringCodec[Int] @@ StringCodec.wrappedString("[", "]")).toJsonCodec),
        roundTripJsonTest("\"{\\\"a\\\":1}\"", Example1(1, None))(JsonCodec[Example1].toStringCodec.toJsonCodec),
        roundTripJsonTest("\"eyJhIjoxfQ\"", Example1(1, None))((JsonCodec[Example1].toStringCodec @@ StringCodec.base64NoPadding).toJsonCodec),
        roundTripJsonTest("\"{\\\"a\\\":1}\"", InJsonString(Example1(1, None)))(JsonCodec[InJsonString[Example1]]),
      ),
      suite("roundTripString")(
        roundTripStringTest("\"hello\"", "hello")(JsonCodec[String].toStringCodec),
        roundTripStringTest("5", 5)(JsonCodec[Int].toStringCodec),
        roundTripStringTest("{\"a\":1}", Example1(1, None))(JsonCodec[Example1].toStringCodec),
        roundTripStringTest("eyJhIjoxfQ", Example1(1, None))(JsonCodec[Example1].toStringCodec @@ StringCodec.base64NoPadding),
      ),
    )

}
