package oxygen.executable

import zio.*

// FIX-PRE-MERGE (KR) : variance
abstract class CliApp[RequiredEnv, ProvidedEnv] {

  final type FullEnv = RequiredEnv & ProvidedEnv

  final type Effect = RIO[FullEnv, Unit | ExitCode]
  final type EnvLayer = RLayer[RequiredEnv, ProvidedEnv]
  final type SubApp = CliApp[? <: FullEnv, ?]

}
object CliApp {

  abstract class Executable[A](using app: DeriveCliApp.Root[A]) extends ZIOAppDefault {

    def defaultLoggerType: DefaultLoggerType = DefaultLoggerType.default

    /**
      * At this point, all [[zio.Cause]] should be caught, and any errors printed.
      */
    private final def runBuiltin(args: List[String]): URIO[Scope, ExitCode] =
      AutoComplete
        .handleArgs(args, app.app).as(ExitCode.success)
        .catchAll {
          case AutoCompleteError.Help(m)         => Console.printLine(m).orDie.as(ExitCode.success)
          case AutoCompleteError.ProgramError(c) => Console.printLine(c.getMessage).orDie.as(ExitCode.failure)
        }

    /**
      * At this point, all [[zio.Cause]] should be caught, and any errors printed.
      */
    private final def runApp(args: List[String]): URIO[Scope, ExitCode] =
      app.app.run(args).catchAllCause(cause => Console.printLine(cause.prettyPrint).orDie.as(ExitCode.failure))

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
