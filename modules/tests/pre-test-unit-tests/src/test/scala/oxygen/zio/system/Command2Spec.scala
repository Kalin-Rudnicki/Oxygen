package oxygen.zio.system

import oxygen.predef.test.*
import oxygen.zio.error.CommandError

object Command2Spec extends OxygenSpecDefault {

  private def builtOf(cmd: Command2): BuiltCommand = cmd.build

  override def testSpec: TestSpec =
    suite("Command2Spec")(
      suite("builder")(
        test("flattens args of varying shape") {
          val built = builtOf(
            Command2("command")(
              "1",
              Option.when(true)("2"),
              Option.when(false)("3"),
              Seq("4", "5"),
              Option.when(true)(Seq("6", "7")),
              Option.when(false)(Seq("8", "9")),
            ),
          )
          assertTrue(
            built.command == "command",
            built.args == List("1", "2", "4", "5", "6", "7"),
          )
        },
        test("captures env") {
          val built =
            builtOf(
              Command2("cmd")("a")
                .envVar("KEY", "value")
                .addEnv("K2" -> "v2"),
            )
          assertTrue(built.env == Map("KEY" -> "value", "K2" -> "v2"))
        },
        test("sudo prepends sudo and is reflected in fullCommand") {
          val cmd = Command2("apt")("update").sudo
          val built = cmd.build
          assertTrue(
            built.command == "sudo",
            built.args == List("apt", "update"),
            built.commandIgnoreSudo == "apt",
            cmd.fullCommand.to[List] == List("sudo", "apt", "update"),
          )
        },
      ),
      suite("showCommand")(
        test("leaves simple tokens un-quoted") {
          assertTrue(builtOf(Command2("echo")("hello", "world")).showCommand.toString == "echo hello world")
        },
        test("single-quotes tokens containing spaces") {
          assertTrue(builtOf(Command2("echo")("a b", "c")).showCommand.toString == "echo 'a b' c")
        },
        test("quotes empty tokens") {
          assertTrue(builtOf(Command2("x")("")).showCommand.toString == "x ''")
        },
        test("escapes embedded single quotes with the POSIX idiom") {
          assertTrue(builtOf(Command2("x")("a'b")).showCommand.toString == "x 'a'\\''b'")
        },
      ),
      suite("execution")(
        test("executeSync captures stdout and a zero exit code") {
          for {
            res <- Command2("echo")("hello").executeSync()
          } yield assertTrue(
            res.stdOut == "hello",
            res.exitCode == 0,
          )
        },
        test("feeds a constant stdin source") {
          for {
            res <- Command2("cat").executeSync(stdIn = CommandInputSource.Const("piped-in"))
          } yield assertTrue(res.stdOut == "piped-in")
        },
        test("runs in the requested working directory") {
          for {
            root <- Path.of("/").orDie
            res <- Command2("pwd").cwd(root).executeSync()
          } yield assertTrue(res.stdOut == "/")
        },
        test("executeSyncDecodeWith decodes stdout") {
          for {
            n <- Command2("echo")("42").executeSyncDecodeWith() { s => Right(s.toInt) }
          } yield assertTrue(n == 42)
        },
        test("executeCode surfaces a non-zero exit code") {
          for {
            code <- Command2("false").executeCode()
          } yield assertTrue(code != 0)
        },
        test("executeSuccess fails with NonZeroExit on non-zero exit") {
          for {
            res <- Command2("false").executeSuccess().either
          } yield assertTrue(res.left.toOption.exists(_.isInstanceOf[CommandError.NonZeroExit]))
        },
      ),
    )

}
