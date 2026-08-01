package oxygen.zio.system

import oxygen.predef.core.*
import oxygen.zio.error.CommandError
import oxygen.zio.syntax.log.*
import zio.*

trait CommandService {

  def executeSync(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      trim: Boolean,
  )(using Trace): IO[CommandError, (stdOut: String, stdErr: String, exitCode: Int)]

  def executeSyncStreamErr(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdErr: CommandOutputSource,
      trim: Boolean,
  )(using Trace): IO[CommandError, (stdOut: String, exitCode: Int)]

  def executeCode(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdOut: CommandOutputSource,
      stdErr: CommandOutputSource,
  )(using Trace): IO[CommandError, Int]

  ///////  ///////////////////////////////////////////////////////////////

  final def executeCodeSuccess(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdOut: CommandOutputSource,
      stdErr: CommandOutputSource,
  )(using Trace): IO[CommandError, Unit] =
    executeCode(
      command = command,
      stdIn = stdIn,
      stdOut = stdOut,
      stdErr = stdErr,
    ).flatMap {
      case 0        => ZIO.unit
      case exitCode => ZIO.fail { CommandError.NonZeroExit(command, exitCode, None, None) }
    }
  final def executeStringSuccess(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdErr: CommandOutputSource,
      trim: Boolean,
  )(using Trace): IO[CommandError, String] =
    stdErr match {
      case CommandOutputSource.Empty =>
        executeSyncDecodeWith(command = command, stdIn = stdIn, stdErrOnSuccess = CommandOutputSource.Empty, trim = trim) { _.asRight }
      case _ =>
        executeSyncStreamErr(
          command = command,
          stdIn = stdIn,
          stdErr = stdErr,
          trim = trim,
        ).flatMap {
          case (exitCode = 0, stdOut = stdOut)        => ZIO.succeed { stdOut }
          case (exitCode = exitCode, stdOut = stdOut) => ZIO.fail { CommandError.NonZeroExit(command, exitCode, stdOut.someWhen(_.nonEmpty), None) }
        }
    }

  final def executeSyncDecodeWith[A](
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdErrOnSuccess: CommandOutputSource,
      trim: Boolean,
  )(
      decode: String => Either[Error, A],
  )(using Trace): IO[CommandError, A] =
    for {

      rawRes <- executeSync(command = command, stdIn = stdIn, trim = trim)
      optStdOutString: Option[String] = rawRes.stdOut.someWhen(_.nonEmpty)
      optStdErrString: Option[String] = rawRes.stdErr.someWhen(_.nonEmpty)

      _ <- ZIO.fail { CommandError.NonZeroExit(command, rawRes.exitCode, optStdOutString, optStdErrString) }.unlessDiscard { rawRes.exitCode == 0 }
      decodedRes <- decode(rawRes.stdOut) match
        case Right(value) => ZIO.succeed { value }
        case Left(error)  => ZIO.fail { CommandError.DecodingFailure(command, error, optStdOutString, optStdErrString) }

      _ <- ZIO.foreachDiscard(optStdErrString) { stdErrString =>
        stdErrOnSuccess match {
          case CommandOutputSource.Empty                                            => ZIO.unit
          case CommandOutputSource.PipeStdOut                                       => Console.printLine { stdErrString }.orDie
          case CommandOutputSource.PipeStdErr                                       => Console.printLineError { stdErrString }.orDie
          case CommandOutputSource.Log(logLevel, showCommand: ShowCommand.NonEmpty) => ZIO.logAtLevel(logLevel)(stdErrString, Cause.Empty) @@ showCommand.toAspect(command)
          case CommandOutputSource.Log(logLevel, ShowCommand.Empty)                 => ZIO.logAtLevel(logLevel)(stdErrString, Cause.Empty)
        }
      }

    } yield decodedRes

}
object CommandService extends CommandServicePlatformSpecific, CommandServicePlatformSpecificImpl {

  def apply[R, E, A](f: CommandService => ZIO[R, E, A]): ZIO[R, E, A] = current.get.flatMap(f)

  val current: FiberRef[CommandService] = Unsafe.unsafely { FiberRef.unsafe.make { default } }

}
