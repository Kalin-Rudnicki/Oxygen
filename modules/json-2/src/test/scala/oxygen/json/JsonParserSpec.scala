package oxygen.json

import oxygen.predef.test.*
import zio.test.{check, TestAspect}

object JsonParserSpec extends OxygenSpecDefault {

  @scala.annotation.unused
  private def makeTest(input: String)(exp: Json)(using SourceLocation): TestSpec =
    test(input) {
      assert(Json.parse(input))(isRight(equalTo_filteredDiff(exp)))
    }

  override def testSpec: TestSpec =
    suite("JsonParserSpec")(
      test("random") {
        check(Generators.strings.anyJsonGen(5)) { case (json, string) =>
          assert(Json.parse(string))(isRight(equalTo_filteredDiff(json)))
        }
      } @@ TestAspect.samples(100),
    )

}
