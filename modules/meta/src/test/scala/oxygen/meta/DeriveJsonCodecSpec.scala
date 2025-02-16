package oxygen.meta

import oxygen.meta.example.*
import oxygen.predef.test.*
import scala.quoted.*

object DeriveJsonCodecSpec extends OxygenSpecDefault {

  private def makeRoundTripTest[A: {JsonCodec as codec}](label: String)(input: JsonAST, exp: A): TestSpec =
    test(label) {
      assertTrue(
        codec.decode(input) == exp.asRight,
        codec.encode(exp) == input,
      )
    }

  private def makeRoundTripTest[A: {JsonCodec as codec}](label: String)(input: JsonAST, output: JsonAST, exp: A): TestSpec =
    test(label) {
      assertTrue(
        codec.decode(input) == exp.asRight,
        codec.encode(exp) == output,
      )
    }

  given JsonCodec[CaseClass1] = JsonCodec.derived
  given JsonCodec[CaseClass2] = JsonCodec.derived
  given JsonCodec[CaseClass3] = JsonCodec.derived
  given JsonCodec[CaseObject1.type] = JsonCodec.derived
  given JsonCodec[Enum1.Case3.type] = JsonCodec.derived
  given JsonCodec[Enum1] = JsonCodec.derived

  given [A: {JsonEncoder, JsonDecoder}, B: {JsonEncoder, JsonDecoder}] => JsonCodec[SealedTrait3[A, B]] = JsonCodec.derived

  private def productSpec: TestSpec =
    suite("product")(
      suite("CaseClass1")(
        makeRoundTripTest("false")(
          JsonAST.JObject(
            "int" -> JsonAST.JNumber(0),
            "string" -> JsonAST.JString("no"),
            "boolean" -> JsonAST.JBoolean(false),
          ),
          CaseClass1(0, "no", false),
        ),
        makeRoundTripTest("true")(
          JsonAST.JObject(
            "int" -> JsonAST.JNumber(1),
            "string" -> JsonAST.JString("yes"),
            "boolean" -> JsonAST.JBoolean(true),
          ),
          CaseClass1(1, "yes", true),
        ),
      ),
      suite("CaseClass3")(
        makeRoundTripTest("present - some")(
          JsonAST.JObject(
            "int" -> JsonAST.JNumber(1),
            "booleans" -> JsonAST.JArray.of(JsonAST.JBoolean(true), JsonAST.JBoolean(false)),
          ),
          CaseClass3(1.some, Seq(true, false).some),
        ),
        makeRoundTripTest("present - none")(
          JsonAST.JObject(
            "int" -> JsonAST.JNull,
            "booleans" -> JsonAST.JNull,
          ),
          JsonAST.JObject(
          ),
          CaseClass3(None, None),
        ),
        makeRoundTripTest("not present")(
          JsonAST.JObject(
          ),
          CaseClass3(None, None),
        ),
      ),
      makeRoundTripTest("CaseClass2")(
        JsonAST.JObject(),
        CaseClass2(),
      ),
      makeRoundTripTest("CaseObject1")(
        JsonAST.JObject(),
        CaseObject1,
      ),
      makeRoundTripTest[Enum1.Case3.type]("Enum1.Case3")(
        JsonAST.JObject(),
        Enum1.Case3,
      ),
    )

  private def sumSpec: TestSpec =
    suite("sum")(
      suite("Enum1")(
        makeRoundTripTest[Enum1]("Case1")(
          JsonAST.JObject(
            "Case1" -> JsonAST.JObject(
              "int" -> JsonAST.JNumber(1),
              "string" -> JsonAST.JString("str"),
            ),
          ),
          Enum1.Case1(1, "str"),
        ),
        makeRoundTripTest[Enum1]("Case2")(
          JsonAST.JObject(
            "Case2" -> JsonAST.JObject(),
          ),
          Enum1.Case2(),
        ),
        makeRoundTripTest[Enum1]("Case3")(
          JsonAST.JObject(
            "Case3" -> JsonAST.JObject(),
          ),
          Enum1.Case3,
        ),
      ),
      suite("SealedTrait3")(
        makeRoundTripTest[SealedTrait3[Int, String]]("A")(
          JsonAST.JObject(
            "A" -> JsonAST.JObject(
              "a" -> JsonAST.JNumber(1),
            ),
          ),
          SealedTrait3.A(1),
        ),
        makeRoundTripTest[SealedTrait3[Int, String]]("B")(
          JsonAST.JObject(
            "B" -> JsonAST.JObject(
              "b" -> JsonAST.JString("str"),
            ),
          ),
          SealedTrait3.B("str"),
        ),
        makeRoundTripTest[SealedTrait3[Int, String]]("AB1")(
          JsonAST.JObject(
            "AB1" -> JsonAST.JObject(
              "a" -> JsonAST.JNumber(1),
              "b" -> JsonAST.JString("str"),
            ),
          ),
          SealedTrait3.AB1(1, "str"),
        ),
        makeRoundTripTest[SealedTrait3[Int, String]]("AB1")(
          JsonAST.JObject(
            "AB2" -> JsonAST.JObject(
              "a" -> JsonAST.JString("str"),
              "b" -> JsonAST.JNumber(1),
            ),
          ),
          SealedTrait3.AB2("str", 1),
        ),
        makeRoundTripTest[SealedTrait3[Int, String]]("Neither")(
          JsonAST.JObject(
            "Neither" -> JsonAST.JObject(
            ),
          ),
          SealedTrait3.Neither,
        ),
      ),
    )

  override def testSpec: TestSpec =
    suite("DeriveJsonCodecSpec")(
      productSpec,
      sumSpec,
    )

}
