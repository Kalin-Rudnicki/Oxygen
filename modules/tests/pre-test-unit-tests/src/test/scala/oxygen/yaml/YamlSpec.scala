package oxygen.yaml

import oxygen.predef.test.*

object YamlSpec extends OxygenSpecDefault {

  private def makeSpec(name: String)(str: String)(yaml: Yaml)(using Trace, SourceLocation): TestSpec =
    test(name) {
      assert(Yaml.fromString(str))(isRight(equalTo(yaml)))
    }

  override def testSpec: TestSpec =
    suite("YamlSpec")(
      makeSpec("empty-1")("")(Yaml.Empty),
      makeSpec("empty-2")("   \n \n   ")(Yaml.Empty),
    )

}
