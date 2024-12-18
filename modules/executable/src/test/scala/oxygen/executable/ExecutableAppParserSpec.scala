package oxygen.executable

import oxygen.cli.*
import oxygen.executable.ExecutableApp.Config.Source.*
import oxygen.predef.test.*
import zio.json.ast.Json

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
        makePassingTest("empty")()(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List())),
        makePassingTest("file")("-f=file.json")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(File("file.json")))),
        makePassingTest("jar resource")("-j=jar.json")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(JarResource("jar.json")))),
        makePassingTest("env var")("-e=ENV_VAR")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(EnvVar(Nil, "ENV_VAR")))),
        makePassingTest("env var (with nesting)")("-e=a.b.c:ENV_VAR")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(EnvVar(List("a", "b", "c"), "ENV_VAR")))),
        makePassingTest("raw (valid json)")("-r={}")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(Raw(Nil, Json.Obj())))),
        makePassingTest("raw (invalid json)")("-r=raw")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(Raw(Nil, Json.Str("raw"))))),
        makePassingTest("raw (valid json + with nesting)")("-r=a.b.c:[1]")(
          ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(Raw(List("a", "b", "c"), Json.Arr(Json.Num(1))))),
        ),
        makePassingTest("raw (invalid json + with nesting)")("-r=a.b.c:raw")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.Oxygen, List(Raw(List("a", "b", "c"), Json.Str("raw"))))),
        makePassingTest("zio default")("--default-logger=zio")(ExecutableApp.Config(ExecutableApp.Config.InitialLoggerDefault.ZIO, List())),
      ),
    )

}
