package oxygen.json

import java.time.*
import oxygen.predef.test.{*, given}

object JsonSpec extends OxygenSpecDefault {

  private def directRoundTripTest[A: JsonCodec](json: String)(encoded: A)(using tt: TypeTag[A], loc: SourceLocation): TestSpec =
    test(s"$tt -> $json") {
      assert(JsonCodec[A].decoder.decodeJsonString(json))(isRight(equalTo_filteredDiff(encoded))) &&
      assert(JsonCodec[A].encoder.encodeJsonStringCompact(encoded))(equalTo(json))
    }

  private def failedDecodeTest[A: JsonDecoder](json: String)(using tt: TypeTag[A], loc: SourceLocation): TestSpec =
    test(s"<fails> $tt -> $json") {
      assert(JsonDecoder[A].decodeJsonString(json))(isLeft)
    }

  // @oxygen.meta.annotation.showDerivation
  final case class Product1(
      s: String,
      b: Boolean,
      i: Option[Int],
  ) derives JsonCodec

  final case class Product2(
      name: String,
      inner: Specified[Product2],
  ) derives JsonCodec

  enum Sum1 derives JsonCodec {
    case S(s: String)
    case B(b: Boolean)
    case I(i: Option[Int])
    case P(p: Product1)
  }

  override def testSpec: TestSpec =
    suite("JsonSpec")(
      suite("provided instances")(
        directRoundTripTest("\"ABC\"")("ABC"),
        directRoundTripTest("123")(123),
        directRoundTripTest("true")(true),
        directRoundTripTest("\"July\"")(Month.JULY),
        directRoundTripTest("true")(true.some),
        directRoundTripTest("false")(false.some),
        directRoundTripTest("null")(Option.empty[Boolean]),
        suite("seq")(
          directRoundTripTest("[]")(Contiguous.empty[Int]),
          directRoundTripTest("[]")(List.empty[Int]),
          directRoundTripTest("[]")(Vector.empty[Int]),
          directRoundTripTest("[1,2,3]")(Contiguous(1, 2, 3)),
          directRoundTripTest("[1,2,3]")(List(1, 2, 3)),
          directRoundTripTest("[1,2,3]")(Vector(1, 2, 3)),
          failedDecodeTest[NonEmptyList[Int]]("[]"),
        ),
      ),
      suite("derived instances")(
        suite("Product1")(
          directRoundTripTest("""{"s":"ABC","b":true,"i":123}""")(Product1("ABC", true, 123.some)),
          directRoundTripTest("""{"s":"ABC","b":true}""")(Product1("ABC", true, None)),
        ),
        suite("Product2")(
          directRoundTripTest("""{"name":"A"}""")(Product2("A", Specified.WasNotSpecified)),
          directRoundTripTest("""{"name":"A","inner":{"name":"B"}}""")(Product2("A", Product2("B", Specified.WasNotSpecified))),
          directRoundTripTest("""{"name":"A","inner":{"name":"B","inner":{"name":"C"}}}""")(Product2("A", Product2("B", Product2("C", Specified.WasNotSpecified)))),
        ),
        suite("Sum1")(
          directRoundTripTest("""{"S":{"s":"ABC"}}""")(Sum1.S("ABC")),
          directRoundTripTest("""{"B":{"b":true}}""")(Sum1.B(true)),
          directRoundTripTest("""{"I":{"i":123}}""")(Sum1.I(123.some)),
          directRoundTripTest("""{"I":{}}""")(Sum1.I(None)),
          directRoundTripTest("""{"P":{"p":{"s":"ABC","b":false}}}""")(Sum1.P(Product1("ABC", false, None))),
        ),
      ),
    )

}
