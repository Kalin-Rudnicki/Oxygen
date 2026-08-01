package oxygen.zio.system

import oxygen.predef.test.*
import oxygen.zio.error.CommandError

object CommandSpec extends OxygenSpecDefault {

  private def builtOf(cmd: Command): BuiltCommand = cmd.build
  private def shownOf(cmd: Command): String = cmd.build.showCommand.toString

  override def testSpec: TestSpec =
    suite("CommandSpec")(
      suite("builder")(
        test("flattens args of varying shape") {
          val built = builtOf(
            Command("command")(
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
              Command("cmd")("a")
                .envVar("KEY", "value")
                .addEnv("K2" -> "v2"),
            )
          assertTrue(built.env == Map("KEY" -> "value", "K2" -> "v2"))
        },
        test("sudo prepends sudo and is reflected in fullCommand") {
          val cmd = Command("apt")("update").sudo
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
          assertTrue(shownOf(Command("echo")("hello", "world")) == "echo hello world")
        },
        test("leaves shell-safe punctuation un-quoted") {
          assertTrue(shownOf(Command("run")("--flag=value", "a/b/c.txt", "1.2.3", "user@host:port")) == "run --flag=value a/b/c.txt 1.2.3 user@host:port")
        },
        test("single-quotes tokens containing spaces") {
          assertTrue(shownOf(Command("echo")("a b", "c")) == "echo 'a b' c")
        },
        test("quotes empty tokens") {
          assertTrue(shownOf(Command("x")("")) == "x ''")
        },
        test("quotes tokens with shell metacharacters") {
          assertTrue(
            shownOf(Command("x")("$HOME")) == "x '$HOME'",
            shownOf(Command("x")("a*b")) == "x 'a*b'",
            shownOf(Command("x")("a`b`")) == "x 'a`b`'",
          )
        },
        test("escapes embedded single quotes with the POSIX idiom") {
          assertTrue(shownOf(Command("x")("a'b")) == "x 'a'\\''b'")
        },
        test("quotes tokens containing a single backslash") {
          // a bare `a\b` would be mangled by the shell to `ab`, so it must be quoted
          assertTrue(shownOf(Command("x")("a\\b")) == "x 'a\\b'")
        },
        test("quotes tokens containing a double backslash") {
          assertTrue(shownOf(Command("x")("a\\\\b")) == "x 'a\\\\b'")
        },
        test("handles a backslash next to a single quote") {
          // value `a\'b` -> single quote is escaped, backslash stays literal inside the quotes
          assertTrue(shownOf(Command("x")("a\\'b")) == "x 'a\\'\\''b'")
        },
      ),
      suite("execution")(
        test("executeSync captures stdout and a zero exit code") {
          for {
            res <- Command("echo")("hello").executeSync()
          } yield assertTrue(
            res.stdOut == "hello",
            res.exitCode == 0,
          )
        },
        test("feeds a constant stdin source") {
          for {
            res <- Command("cat").executeSync(stdIn = CommandInputSource.Const("piped-in"))
          } yield assertTrue(res.stdOut == "piped-in")
        },
        test("runs in the requested working directory") {
          for {
            root <- Path.of("/").orDie
            res <- Command("pwd").cwd(root).executeSync()
          } yield assertTrue(res.stdOut == "/")
        },
        test("executeSyncDecodeWith decodes stdout") {
          for {
            n <- Command("echo")("42").executeSyncDecodeWith() { s => Right(s.toInt) }
          } yield assertTrue(n == 42)
        },
        test("executeCode surfaces a non-zero exit code") {
          for {
            code <- Command("false").executeCode()
          } yield assertTrue(code != 0)
        },
        test("executeSuccess fails with NonZeroExit on non-zero exit") {
          for {
            res <- Command("false").executeSuccess().either
          } yield assertTrue(res.left.toOption.exists(_.isInstanceOf[CommandError.NonZeroExit]))
        },
      ),
    )

}
