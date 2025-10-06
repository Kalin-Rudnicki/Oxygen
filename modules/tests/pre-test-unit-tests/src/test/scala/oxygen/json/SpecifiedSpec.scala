package oxygen.json

import oxygen.predef.json.*
import oxygen.predef.test.*

object SpecifiedSpec extends OxygenSpecDefault {

  final case class WrappedInt(i: Int)
  object WrappedInt {
    given Conversion[Int, WrappedInt] = WrappedInt(_)
    given JsonCodec[WrappedInt] = JsonCodec.int.transform(WrappedInt(_), _.i)
  }

  final case class MyClass(
      i: Specified[Int],
      s: Specified[Option[String]],
      b: Specified[List[Boolean]],
      w: Specified[WrappedInt],
  ) derives JsonCodec
  object MyClass {

    def make(
        i: Specified[Int] = Specified.WasNotSpecified,
        s: Specified[Option[String]] = Specified.WasNotSpecified,
        b: Specified[List[Boolean]] = Specified.WasNotSpecified,
        w: Specified[WrappedInt] = Specified.WasNotSpecified,
    ): MyClass =
      MyClass(
        i = i,
        s = s,
        b = b,
        w = w,
      )

  }

  private def makeDecodeTest(input: String)(exp: MyClass)(using SourceLocation): TestSpec =
    test(input) {
      assertTrue(input.fromJsonString[MyClass] == exp.asRight)
    }

  private def roundTripTest(exp: MyClass)(using SourceLocation): TestSpec =
    test(exp.toString) {
      assertTrue(exp.toJsonStringCompact.fromJsonString[MyClass] == exp.asRight)
    }

  override def testSpec: TestSpec =
    suite("SpecifiedSpec")(
      suite("decode")(
        makeDecodeTest(""" {} """)(MyClass.make()),
        makeDecodeTest(""" { "i": 1 } """)(MyClass.make(i = 1)),
        makeDecodeTest(""" { "s": "s" } """)(MyClass.make(s = "s".some)),
        makeDecodeTest(""" { "s": null } """)(MyClass.make(s = None)),
        makeDecodeTest(""" { "b": [] } """)(MyClass.make(b = Nil)),
        makeDecodeTest(""" { "b": [true, false] } """)(MyClass.make(b = List(true, false))),
        makeDecodeTest(""" { "w": 25 } """)(MyClass.make(w = 25)),
        makeDecodeTest(""" { "i": 1, "s": "s", "b": [true, false] } """)(MyClass.make(i = 1, s = "s".some, b = List(true, false))),
      ),
      suite("round trip")(
        roundTripTest(MyClass.make()),
        roundTripTest(MyClass.make(i = 1)),
        roundTripTest(MyClass.make(s = "s".some)),
        roundTripTest(MyClass.make(s = None)),
        roundTripTest(MyClass.make(b = Nil)),
        roundTripTest(MyClass.make(b = List(true, false))),
        roundTripTest(MyClass.make(i = 1, s = "s".some, b = List(true, false))),
      ),
    )

}
