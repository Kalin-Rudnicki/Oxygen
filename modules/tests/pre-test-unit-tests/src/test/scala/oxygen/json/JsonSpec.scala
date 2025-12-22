package oxygen.json

import java.time.*
import oxygen.meta.k0.*
import oxygen.predef.test.*

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

  final case class Product3(
      @jsonField("f1") a: Int,
      @jsonField("f2") b: String,
  ) derives JsonCodec

  final case class WrappedBoolean(internal: Boolean)
  object WrappedBoolean {
    given JsonCodec[WrappedBoolean] = JsonCodec.deriveWrapped
  }

  enum Sum1 derives JsonCodec {
    case S(s: String)
    case B(b: Boolean)
    case I(i: Option[Int])
    case P(p: Product1)
  }

  sealed trait Sum2 derives JsonCodec
  object Sum2 {
    @jsonType("c1")
    final case class Case1(@jsonField("f1") a: Int) extends Sum2
    final case class Case2(b: Boolean) extends Sum2
  }

  // @oxygen.meta.K0.annotation.showDerivation[JsonDecoder]
  @jsonDiscriminator("type")
  sealed trait Sum3 derives JsonCodec
  object Sum3 {
    @jsonType("c1")
    final case class Case1(@jsonField("f1") a: Int) extends Sum3
    final case class Case2(b: Boolean) extends Sum3
  }

  final case class MyClass1(`type`: String, value: String)
  object MyClass1 {
    given JsonCodec[MyClass1] = JsonCodec.jsonStringFromStringCodec { StringCodec.fromDerivedJsonCodec[MyClass1] @@ StringCodec.base64UrlNoPadding }
  }

  final case class MyClass2(field: MyClass1) derives JsonCodec

  object NestedSums {

    sealed trait UnrollNoDiscriminator derives JsonCodec
    object UnrollNoDiscriminator {

      sealed trait Case1 extends UnrollNoDiscriminator
      sealed trait Case2 extends UnrollNoDiscriminator

      case object A extends Case1
      case object B extends Case1
      case object C extends Case2
      case object D extends Case2

    }

    @jsonDiscriminator("type")
    sealed trait UnrollWithDiscriminator derives JsonCodec
    object UnrollWithDiscriminator {

      sealed trait Case1 extends UnrollWithDiscriminator
      sealed trait Case2 extends UnrollWithDiscriminator

      case object A extends Case1
      case object B extends Case1
      case object C extends Case2
      case object D extends Case2

    }

    @overrideAllUnrollStrategy(SumGeneric.UnrollStrategy.Nested)
    sealed trait NestedNoDiscriminator derives JsonCodec
    object NestedNoDiscriminator {

      sealed trait Case1 extends NestedNoDiscriminator
      sealed trait Case2 extends NestedNoDiscriminator

      case object A extends Case1
      case object B extends Case1
      case object C extends Case2
      case object D extends Case2

    }

    @overrideAllUnrollStrategy(SumGeneric.UnrollStrategy.Nested)
    @jsonDiscriminator("type1")
    sealed trait NestedWithDiscriminator derives JsonCodec
    object NestedWithDiscriminator {

      @jsonDiscriminator("type2")
      sealed trait Case1 extends NestedWithDiscriminator
      @jsonDiscriminator("type3")
      sealed trait Case2 extends NestedWithDiscriminator

      case object A extends Case1
      case object B extends Case1
      case object C extends Case2
      case object D extends Case2

    }

  }

  final case class FlattenOuterRequired(
      outer1: Int,
      outer2: Option[String],
      @jsonFlatten inner: FlattenInner,
  ) derives JsonCodec

  final case class FlattenInner(
      inner1: Int,
      inner2: Option[String],
  ) derives JsonCodec

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
          directRoundTripTest("[]")(Seq.empty[Int]),
          directRoundTripTest("[]")(List.empty[Int]),
          directRoundTripTest("[]")(Vector.empty[Int]),
          directRoundTripTest("[1,2,3]")(Seq(1, 2, 3)),
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
        suite("Product3")(
          directRoundTripTest("""{"f1":1,"f2":"hi"}""")(Product3(1, "hi")),
        ),
        suite("WrappedBoolean")(
          directRoundTripTest("""true""")(WrappedBoolean(true)),
        ),
        suite("Sum1")(
          directRoundTripTest("""{"S":{"s":"ABC"}}""")(Sum1.S("ABC")),
          directRoundTripTest("""{"B":{"b":true}}""")(Sum1.B(true)),
          directRoundTripTest("""{"I":{"i":123}}""")(Sum1.I(123.some)),
          directRoundTripTest("""{"I":{}}""")(Sum1.I(None)),
          directRoundTripTest("""{"P":{"p":{"s":"ABC","b":false}}}""")(Sum1.P(Product1("ABC", false, None))),
        ),
        suite("Sum2")(
          directRoundTripTest[Sum2]("""{"c1":{"f1":1}}""")(Sum2.Case1(1)),
          directRoundTripTest[Sum2]("""{"Case2":{"b":true}}""")(Sum2.Case2(true)),
        ),
        suite("Sum3")(
          directRoundTripTest[Sum3]("""{"type":"c1","f1":1}""")(Sum3.Case1(1)),
          directRoundTripTest[Sum3]("""{"type":"Case2","b":true}""")(Sum3.Case2(true)),
        ),
        suite("NestedSums")(
          suite("UnrollNoDiscriminator")(
            directRoundTripTest[NestedSums.UnrollNoDiscriminator]("""{"A":{}}""")(NestedSums.UnrollNoDiscriminator.A),
            directRoundTripTest[NestedSums.UnrollNoDiscriminator]("""{"B":{}}""")(NestedSums.UnrollNoDiscriminator.B),
            directRoundTripTest[NestedSums.UnrollNoDiscriminator]("""{"C":{}}""")(NestedSums.UnrollNoDiscriminator.C),
            directRoundTripTest[NestedSums.UnrollNoDiscriminator]("""{"D":{}}""")(NestedSums.UnrollNoDiscriminator.D),
          ),
          suite("UnrollWithDiscriminator")(
            directRoundTripTest[NestedSums.UnrollWithDiscriminator]("""{"type":"A"}""")(NestedSums.UnrollWithDiscriminator.A),
            directRoundTripTest[NestedSums.UnrollWithDiscriminator]("""{"type":"B"}""")(NestedSums.UnrollWithDiscriminator.B),
            directRoundTripTest[NestedSums.UnrollWithDiscriminator]("""{"type":"C"}""")(NestedSums.UnrollWithDiscriminator.C),
            directRoundTripTest[NestedSums.UnrollWithDiscriminator]("""{"type":"D"}""")(NestedSums.UnrollWithDiscriminator.D),
          ),
          suite("NestedNoDiscriminator")(
            directRoundTripTest[NestedSums.NestedNoDiscriminator]("""{"Case1":{"A":{}}}""")(NestedSums.NestedNoDiscriminator.A),
            directRoundTripTest[NestedSums.NestedNoDiscriminator]("""{"Case1":{"B":{}}}""")(NestedSums.NestedNoDiscriminator.B),
            directRoundTripTest[NestedSums.NestedNoDiscriminator]("""{"Case2":{"C":{}}}""")(NestedSums.NestedNoDiscriminator.C),
            directRoundTripTest[NestedSums.NestedNoDiscriminator]("""{"Case2":{"D":{}}}""")(NestedSums.NestedNoDiscriminator.D),
          ),
          suite("NestedWithDiscriminator")(
            directRoundTripTest[NestedSums.NestedWithDiscriminator]("""{"type1":"Case1","type2":"A"}""")(NestedSums.NestedWithDiscriminator.A),
            directRoundTripTest[NestedSums.NestedWithDiscriminator]("""{"type1":"Case1","type2":"B"}""")(NestedSums.NestedWithDiscriminator.B),
            directRoundTripTest[NestedSums.NestedWithDiscriminator]("""{"type1":"Case2","type3":"C"}""")(NestedSums.NestedWithDiscriminator.C),
            directRoundTripTest[NestedSums.NestedWithDiscriminator]("""{"type1":"Case2","type3":"D"}""")(NestedSums.NestedWithDiscriminator.D),
          ),
        ),
        suite("FlattenOuterRequired")(
          directRoundTripTest("""{"outer1":1,"inner1":2}""")(FlattenOuterRequired(1, None, FlattenInner(2, None))),
          directRoundTripTest("""{"outer1":1,"outer2":"O","inner1":2,"inner2":"I"}""")(FlattenOuterRequired(1, "O".some, FlattenInner(2, "I".some))),
        ),
      ),
      suite("string transform")(
        directRoundTripTest("""{"field":"eyJ0eXBlIjoiYmFzZTY0IiwidmFsdWUiOiJTdHJpbmcifQ"}""")(MyClass2(MyClass1("base64", "String"))),
      ),
    )

}
