package oxygen.executable

import oxygen.cli.*
import oxygen.predef.test.*
import zio.*

// Root app: constructor params are not parsed (roots are zero-arg) — `count` moves to @execute here.
final case class V2Greeter() extends CliApp[Any, Any] {
  @execute
  def run(@named count: Int, @positional name: String, @flag loud: Boolean): Effect =
    ZIO.succeed(ExitCode(count + name.length + (if loud then 1 else 0)))
}
object V2Greeter { given CliApp.Derived[V2Greeter, Any] = CliApp.derive }

// Env layer: `def env` provides a service that the effect consumes.
trait V2Greeting { def text: String }
final case class V2EnvApp() extends CliApp[Any, V2Greeting] {
  def env: EnvLayer = ZLayer.succeed(new V2Greeting { def text = "hi" })
  @execute
  def run(): Effect = ZIO.serviceWith[V2Greeting](g => ExitCode(g.text.length))
}
object V2EnvApp { given CliApp.Derived[V2EnvApp, Any] = CliApp.derive }

// Sub-commands.
final case class V2Calc() extends CliApp[Any, Any] {
  @command def add(@positional a: Int, @positional b: Int): Effect = ZIO.succeed(ExitCode(a + b))
  @command def half(@positional a: Int): Effect = ZIO.succeed(ExitCode(a / 2))
}
object V2Calc { given CliApp.Derived[V2Calc, Any] = CliApp.derive }

// Nested sub-app: a @command that returns another CliApp (which derives itself).
final case class V2Inner() extends CliApp[Any, Any] {
  @execute
  def run(@positional x: Int, @flag verbose: Boolean): Effect = ZIO.succeed(ExitCode(if verbose then x + 1 else x))
}
object V2Inner { given CliApp.Derived[V2Inner, Any] = CliApp.derive }

final case class V2Outer() extends CliApp[Any, Any] {
  @command def inner: V2Inner = V2Inner()
}
object V2Outer { given CliApp.Derived[V2Outer, Any] = CliApp.derive }

// Three named params sharing a first char ('c'): auto short-name resolution must give `-c` to exactly one.
final case class V2ShortCollision() extends CliApp[Any, Any] {
  @execute
  def run(@named count: Int, @named color: String, @flag caps: Boolean): Effect =
    ZIO.succeed(ExitCode(count + color.length + (if caps then 1 else 0)))
}
object V2ShortCollision { given CliApp.Derived[V2ShortCollision, Any] = CliApp.derive }

object DeriveCliAppSpec extends OxygenSpecDefault {

  private def appOf[A](using d: CliApp.Derived[A, Any]): CompiledCliApp[Unit, Any] = d.app

  private def run(app: CompiledCliApp[Unit, Any], args: String*): ZIO[Scope, ExecutableError, ExitCode] =
    Args.parse(args.toList) match
      case Right(a)  => app.execute((), a)
      case Left(msg) => ZIO.dieMessage(s"failed to tokenize: $msg")

  override def testSpec: TestSpec =
    suite("DeriveCliAppSpec")(
      suite("root @execute")(
        test("runs the effect with parsed args") {
          ZIO.scoped(run(appOf[V2Greeter], "abc", "--count", "2", "--loud")).map(ec => assertTrue(ec == ExitCode(6)))
        },
        test("flag defaults to false when absent") {
          ZIO.scoped(run(appOf[V2Greeter], "x", "--count", "10")).map(ec => assertTrue(ec == ExitCode(11)))
        },
        test("missing required positional -> help + usage-error exit") {
          ZIO.scoped(run(appOf[V2Greeter], "--count", "2")).map(ec => assertTrue(ec == ExecutableError.usageErrorExitCode))
        },
      ),
      suite("env layer (def env -> prependLayer)")(
        test("effect sees the provided service") {
          ZIO.scoped(run(appOf[V2EnvApp])).map(ec => assertTrue(ec == ExitCode(2)))
        },
      ),
      suite("sub-commands (@command)")(
        test("dispatches add") {
          ZIO.scoped(run(appOf[V2Calc], "add", "2", "3")).map(ec => assertTrue(ec == ExitCode(5)))
        },
        test("dispatches half") {
          ZIO.scoped(run(appOf[V2Calc], "half", "8")).map(ec => assertTrue(ec == ExitCode(4)))
        },
        test("unknown command -> usage-error exit") {
          ZIO.scoped(run(appOf[V2Calc], "nope")).map(ec => assertTrue(ec == ExecutableError.usageErrorExitCode))
        },
      ),
      suite("nested sub-app (@command returning a CliApp)")(
        test("dispatches into the nested app's @execute") {
          ZIO.scoped(run(appOf[V2Outer], "inner", "7")).map(ec => assertTrue(ec == ExitCode(7)))
        },
      ),
      suite("auto short-name collision resolution")(
        test("only the first colliding param wins `-c`; it parses to that param") {
          // `-c 4` must set `count` (the first 'c' param), not `color`.
          ZIO.scoped(run(appOf[V2ShortCollision], "-c", "4", "--color", "red")).map(ec => assertTrue(ec == ExitCode(7)))
        },
        test("the losing params are still reachable by their long names") {
          ZIO.scoped(run(appOf[V2ShortCollision], "--count", "4", "--color", "red", "--caps")).map(ec => assertTrue(ec == ExitCode(8)))
        },
        test("help shows a single `-c`, on `--count` only") {
          val help = appOf[V2ShortCollision].helpFor(Nil, Nil, HelpType.Help).toString
          assertTrue(
            help.contains("--count, -c"),
            help.contains("--color"),
            !help.contains("--color, -c"),
            !help.contains("--caps, -c"),
          )
        },
      ),
      suite("completion")(
        test("offers all sub-command names (plus built-in flags) at the command position") {
          appOf[V2Calc].complete(CompletionRequest(1, 0, List(""), " ")).map { out =>
            assertTrue(out.contains("add"), out.contains("half"), out.contains("--help"))
          }
        },
        test("filters sub-command names by the partial token") {
          appOf[V2Calc].complete(CompletionRequest(1, 0, List("a"), " ")).map(out => assertTrue(out == List("add")))
        },
        test("offers flag names for a leaf app") {
          appOf[V2Greeter].complete(CompletionRequest(1, 0, List("--"), " ")).map(out => assertTrue(out.contains("--count") && out.contains("--loud")))
        },
        test("drills into a sub-command and completes its flags") {
          appOf[V2Outer].complete(CompletionRequest(2, 1, List("inner", "--"), " ")).map(out => assertTrue(out.contains("--verbose")))
        },
      ),
      suite("help")(
        test("--help on a group lists sub-commands without their args") {
          val help = appOf[V2Calc].helpFor(Nil, Nil, HelpType.Help).toString
          assertTrue(help.contains("add"), help.contains("half"), !help.contains("[a]"), !help.contains("[b]"))
        },
        test("--help-extra on a group expands each sub-command's args") {
          val help = appOf[V2Calc].helpFor(Nil, Nil, HelpType.HelpExtra).toString
          assertTrue(help.contains("add"), help.contains("half"), help.contains("[a]"), help.contains("[b]"))
        },
        test("--help-extra expands a nested sub-app's leaf args") {
          val help = appOf[V2Outer].helpFor(Nil, Nil, HelpType.HelpExtra).toString
          assertTrue(help.contains("inner"), help.contains("[x]"), help.contains("--verbose"))
        },
        test("drilling into a specific command shows its args under plain --help") {
          val help = appOf[V2Calc].helpFor(Nil, List("add"), HelpType.Help).toString
          assertTrue(help.contains("[a]"), help.contains("[b]"))
        },
      ),
    )

}
