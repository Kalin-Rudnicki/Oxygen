package oxygen.schema

import oxygen.meta.annotation.showDerivation
import oxygen.predef.test.*

object DeriveProductSchemaSpec extends OxygenSpecDefault {

  @showDerivation
  final case class MyThing(
      a: String,
      b: String,
  ) derives JsonSchema

  private def makeEncodingTest[A: JsonSchema](a: A, indent: Option[Int])(exp: String)(using SourceLocation): TestSpec =
    test((a, indent).toString) {
      println(
        s"""
           |${JsonSchema[A].jsonCodec.encodeJson(a, indent)}
           |$exp
           |""".stripMargin,
      )
      assert(JsonSchema[A].jsonCodec.encodeJson(a, indent))(equalTo(exp))
    }

  override def testSpec: TestSpec =
    suite("DeriveProductSchemaSpec")(
      suite("MyThing")(
        makeEncodingTest(MyThing("a-value", "b-value"), None)(
          """{"a":"a-value","b":"b-value"}""",
        ),
      ),
    )

}
