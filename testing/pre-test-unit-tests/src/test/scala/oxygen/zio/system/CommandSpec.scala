package oxygen.zio.system

import oxygen.predef.test.*
import zio.test.ZTestLogger

object CommandSpec extends OxygenSpecDefault {

  private def argParseTest(name: String)(arg: Command.Arg, exp: List[String])(using SourceLocation): TestSpec =
    test(name) {
      assertTrue(Command.Arg.unapply(arg).value == exp)
    }

  override def testSpec: TestSpec =
    suite("CommandSpec")(
      suite("args")(
        argParseTest("None")(None, Nil),
        argParseTest("String")("1", List("1")),
        argParseTest("Some[String]")(Option.when(true)("1"), List("1")),
        argParseTest("None[String]")(Option.when(false)("1"), Nil),
        argParseTest("Seq[String]")(Seq("1", "2"), List("1", "2")),
        argParseTest("Some[Seq[String]]")(Option.when(true)(Seq("1", "2")), List("1", "2")),
        argParseTest("None[Seq[String]]")(Option.when(false)(Seq("1", "2")), Nil),
      ),
      suite("command")(
        test("command builds properly") {
          assertTrue(
            Command("command")(
              "1",
              Option.when(true)("2"),
              Option.when(false)("3"),
              Seq("4", "5"),
              Option.when(true)(Seq("6", "7")),
              Option.when(false)(Seq("8", "9")),
            ).cmd == List("command", "1", "2", "4", "5", "6", "7"),
          )
        },
        test("executes as expected") {
          for {
            _ <- Command("echo")("command-1").executeSuccess(outLevel = LogLevel.Warning)
            _ <- Command("echo")("command-2").executeSuccess(outLevel = LogLevel.Debug)
            failRes <- Command("echo-dne").executeSuccess(outLevel = LogLevel.Debug).exit
            strRes <- Command("echo")("command-3").executeString()
            logs <- ZTestLogger.logOutput
          } yield assertTrue(
            logs.exists { log => log.logLevel == LogLevel.Warning && log.message() == "command-1" && log.annotations.get("command").contains("echo") },
            logs.exists { log => log.logLevel == LogLevel.Debug && log.message() == "command-2" && log.annotations.get("command").contains("echo") },
            strRes.trim == "command-3",
          ) && assert(failRes)(fails(anything))
        },
      ),
    )

}
