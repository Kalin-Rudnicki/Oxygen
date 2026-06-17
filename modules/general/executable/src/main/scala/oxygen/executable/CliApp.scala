package oxygen.executable

import oxygen.cli.*
import zio.*

// FIX-PRE-MERGE (KR) : remove
final case class MyThing1() extends CliApp[Any, Int & Boolean] derives CompiledCliApp.DeriveRootApp
final case class MyThing2() extends CliApp[String, Int & Boolean] derives CompiledCliApp.DeriveSubApp.Aux
object MyThing2 {
  // given CompiledCliApp.DeriveSubApp.Extract[MyThing2] = CompiledCliApp.DeriveSubApp.derived[MyThing2]
}

object Other {

  // oxygen.quoted.MacroUtil.showExpr { summon[CompiledCliApp.DeriveSubApp[MyThing2]] }
  oxygen.quoted.MacroUtil.showExpr { CompiledCliApp.DeriveSubApp.derived[MyThing2] }
  // summon[CompiledCliApp.DeriveSubApp[MyThing2]]
  // summon[CompiledCliApp.DeriveSubApp.Aux[String, MyThing2]]

}

abstract class CliApp[RequiredEnv, ProvidedEnv] {

  final type FullEnv = RequiredEnv & ProvidedEnv

  final type Effect = RIO[FullEnv & Scope, Unit | ExitCode]
  final type EffectE[E <: Throwable] = ZIO[FullEnv & Scope, E, Unit | ExitCode]
  final type EnvLayer = RLayer[RequiredEnv, ProvidedEnv]
  final type SubApp = CliApp[? <: FullEnv, ?]

}
object CliApp {

  /**
    * Per-file derivation result. Each `CliApp` materializes its own `Derived` in its own file
    * (`given CliApp.Derived[Foo, R] = CliApp.derive`); parents reference a child's `body` rather than
    * recursively reflecting into the child class. `R` is the app's `RequiredEnv`, surfaced as a type
    * parameter so it composes at every site.
    *
    *   - `body` — given an instance of `A`, the runnable app (this app's `def env` already prepended).
    *   - `app`  — `body` with a zero-arg root instance; only valid for zero-arg roots (sub-apps that
    *     take constructor params are built by their parent command, so their `app` is a runtime stub).
    */
  trait Derived[A, R] { // FIX-PRE-MERGE (KR) : remove
    def body: CompiledCliApp[A, R]
    def app: CompiledCliApp[Unit, R]
  }

  inline def derive[A, R]: Derived[A, R] = ${ oxygen.executable.generic.DeriveCliApp.derive[A, R] }

  // `derived` is passed explicitly (`extends CliApp.Executable[Foo](CliApp.derive)`) rather than via a
  // `using` so it can't be a self-referential given on the same object being constructed. `Derived[A, Any]`
  // enforces that a runnable root requires no environment.
  abstract class Executable[A](derived: CliApp.Derived[A, Any]) extends ZIOAppDefault {

    def defaultLoggerType: DefaultLoggerType = DefaultLoggerType.default

    private val app: CompiledCliApp[Unit, Any] = derived.app

    /**
      * At this point, all [[zio.Cause]] should be caught, and any errors printed.
      */
    private final def runBuiltin(args: List[String]): URIO[Scope, ExitCode] =
      AutoComplete
        .handleArgs(args, app).as(ExitCode.success)
        .catchAll {
          case AutoCompleteError.Help(m)         => Console.printLine(m).orDie.as(ExitCode.success)
          case AutoCompleteError.ProgramError(c) => Console.printLine(c.getMessage).orDie.as(ExitCode.failure)
        }

    /**
      * At this point, all [[zio.Cause]] should be caught, and any errors printed.
      */
    private final def runApp(rawArgs: List[String]): URIO[Scope, ExitCode] =
      Args.parse(rawArgs) match
        case Left(message) => Console.printLine(message).orDie.as(ExecutableError.usageErrorExitCode)
        case Right(parsed) =>
          // `peelArgs` strips any of --help / -h / --help-extra / -H from the named args and reports which
          // (if any) was present; the leftover positional args are the command path to drill into.
          CliHelp.peelArgs(parsed) match
            case (stripped, Some(helpType)) =>
              val path = stripped.positional.args.map(_.value)
              val asJson = Option(java.lang.System.getenv(Constants.oxygenCliJson)).flatMap(_.toBooleanOption).getOrElse(false)
              val rendered = if asJson then app.helpJson(Nil, path).showPretty else app.helpFor(Nil, path, helpType).toString
              Console.printLine(rendered).orDie.as(ExitCode.success)
            case (_, None) =>
              app.execute((), parsed).catchAllCause { cause =>
                ExecutableError.ExitWith.exitCodeFromCause(cause) match
                  case Some(code) => ZIO.succeed(code)
                  case None       =>
                    ZIO.logDebugCause("CLI app failed", cause) *>
                      Console.printLine(ExecutableError.ExitWith.messageFromCause(cause)).orDie.as(ExitCode.failure)
              }

    override final def run: URIO[ZIOAppArgs & Scope, Unit] =
      for {
        _ <- CliAppLogging.install(defaultLoggerType)
        rawArgs <- ZIOAppArgs.getArgs
        exitCode <- rawArgs.toList match
          case "--:" :: builtinRest => runBuiltin(builtinRest)
          case args                 => runApp(args)
        doExit <- System.env(Constants.oxygenExecutableExit).!.map(_.flatMap(_.toBooleanOption).getOrElse(true))
        _ <- (doExit, exitCode) match
          case (true, _)            => exit(exitCode)
          case (false, ExitCode(0)) => ZIO.unit // TODO (KR) : do something different?
          case (false, _)           => ZIO.unit // TODO (KR) : do something different?
      } yield ()

  }

}
