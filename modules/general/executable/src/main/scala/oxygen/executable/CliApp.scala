package oxygen.executable

import oxygen.cli.*
import zio.*

// A trait (not a class) so sub-apps can be plain `trait`s / abstract classes — a sub-app is instantiated
// by its parent `@command`, never by the derivation macro, so it need not be a case class.
trait CliApp[RequiredEnv, ProvidedEnv] {

  final type FullEnv = RequiredEnv & ProvidedEnv

  final type Effect = RIO[FullEnv & Scope, Unit | ExitCode]
  final type EffectE[E <: Throwable] = ZIO[FullEnv & Scope, E, Unit | ExitCode]
  final type EnvLayer = RLayer[RequiredEnv, ProvidedEnv]
  final type SubApp = CliApp[? <: FullEnv, ?]

}
object CliApp {

  // The runnable root is derived via `A derives CompiledCliApp.DeriveRootApp` (companion given), summoned
  // here `using`. The `A <: CliApp[Any, ?]` bound enforces that a runnable root requires no environment.
  // `derived` is by-name (+ `app` is lazy) because the given lives on the same companion object that
  // `extends Executable[A]`, so eager capture in the super constructor would be an illegal self-reference.
  abstract class Executable[A <: CliApp[Any, ?]](using derived: => CompiledCliApp.DeriveRootApp[A]) extends ZIOAppDefault {

    def defaultLoggerType: DefaultLoggerType = DefaultLoggerType.default

    private lazy val app: CompiledCliApp[Unit, Any] = derived.app

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
