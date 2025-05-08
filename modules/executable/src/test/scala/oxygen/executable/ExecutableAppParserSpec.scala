package oxygen.executable

import oxygen.cli.*
import oxygen.executable.ExecutableApp.Config.Source.*
import oxygen.predef.json.*
import oxygen.predef.test.*

object ExecutableAppParserSpec extends OxygenSpecDefault {

  private def successValueAssertion[A](valueAssertion: Assertion[A]): Assertion[Parser.FinalParseResult.Success[A]] =
    hasField("value", _.value, valueAssertion)

  private def hasValue[A](value: A): Assertion[Parser.FinalParseResult.Success[A]] =
    successValueAssertion(equalTo(value))

  private def isSuccess[A](assertion: Assertion[Parser.FinalParseResult.Success[A]]): Assertion[Parser.FinalParseResult[A]] =
    isSubtype[Parser.FinalParseResult.Success[A]](assertion)

  private def makePassingTest(label: String)(args: String*)(exp: ExecutableApp.Config)(using SourceLocation): TestSpec =
    test(label) {
      assert(ExecutableApp.Config.parser(args*))(isSuccess(hasValue(exp)))
    }

  override def testSpec: TestSpec =
    suite("ExecutableAppParserSpec")(
      suite("passing")(
        makePassingTest("empty")()(ExecutableApp.Config(List(), false)),
        makePassingTest("file")("-f=file.json")(ExecutableApp.Config(List(File("file.json")), false)),
        makePassingTest("jar resource")("-j=jar.json")(ExecutableApp.Config(List(JarResource("jar.json")), false)),
        makePassingTest("env var")("-e=ENV_VAR")(ExecutableApp.Config(List(EnvVar(Nil, "ENV_VAR")), false)),
        makePassingTest("env var (with nesting)")("-e=a.b.c:ENV_VAR")(ExecutableApp.Config(List(EnvVar(List("a", "b", "c"), "ENV_VAR")), false)),
        makePassingTest("raw (valid json)")("-r={}")(ExecutableApp.Config(List(Raw(Nil, Json.obj())), false)),
        makePassingTest("raw (invalid json)")("-r=raw")(ExecutableApp.Config(List(Raw(Nil, Json.Str("raw"))), false)),
        makePassingTest("raw (valid json + with nesting)")("-r=a.b.c:[1]")(
          ExecutableApp.Config(List(Raw(List("a", "b", "c"), Json.arr(Json.number(1)))), false),
        ),
        makePassingTest("raw (invalid json + with nesting)")("-r=a.b.c:raw")(ExecutableApp.Config(List(Raw(List("a", "b", "c"), Json.Str("raw"))), false)),
        makePassingTest("zio default")("--keep-zio-logger")(ExecutableApp.Config(List(), true)),
        makePassingTest("zio default (short)")("-Z")(ExecutableApp.Config(List(), true)),
      ),
    )

}
