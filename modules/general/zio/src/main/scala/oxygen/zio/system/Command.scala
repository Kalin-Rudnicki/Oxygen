package oxygen.zio.system

import oxygen.json.JsonDecoder
import oxygen.predef.core.*
import oxygen.schema.*
import oxygen.zio.error.CommandError
import oxygen.zio.logging.LogLevels
import zio.*

/**
  * Immutable builder for an external OS command, executed through [[CommandService]].
  *
  * Execution goes through the pluggable [[CommandService]], with first-class stdin/stdout/stderr sources
  * and typed decoding of process output.
  */
final class Command private (isSudo: Boolean, command: String, args: Growable[String], cwdPath: Option[Path], env: Growable[(String, String)]) {

  lazy val fullCommand: Growable[String] =
    if isSudo then "sudo" +: command +: args
    else command +: args

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def sudo: Command = sudoIf(true)
  def sudoIf(cond: Boolean): Command = new Command(cond, command, args, cwdPath, env)

  def apply(args: Command.Args*): Command =
    new Command(isSudo, command, this.args ++ Growable.many(args).flatMap(_.args), cwdPath, env)

  def addEnv(env: Growable[(String, String)]): Command = new Command(isSudo, command, args, cwdPath, this.env ++ env)
  def addEnv(env: (String, String)*): Command = new Command(isSudo, command, args, cwdPath, this.env ++ Growable.many(env))
  def envVar(key: String, value: String): Command = new Command(isSudo, command, args, cwdPath, this.env :+ (key, value))
  def envVar(key: String, value: Option[String]): Command = value.fold(this)(this.envVar(key, _))

  def cwd(file: Path): Command = new Command(isSudo, command, args, file.some, env)
  def cwd(file: Option[Path]): Command = new Command(isSudo, command, args, file, env)

  def build: BuiltCommand = {
    val (finalCommand, finalArgs): (String, List[String]) =
      if isSudo then ("sudo", command :: args.to[List])
      else (command, args.to[List])

    BuiltCommand(
      command = finalCommand,
      args = finalArgs,
      cwd = cwdPath,
      env = env.toArraySeq[(String, String)].toMap,
    )
  }

  /////// API ///////////////////////////////////////////////////////////////

  def executeSync(
      stdIn: CommandInputSource = CommandInputSource.Empty,
      trim: Boolean = true,
  )(using Trace): IO[CommandError, (stdOut: String, stdErr: String, exitCode: Int)] =
    CommandService {
      _.executeSync(
        command = build,
        stdIn = stdIn,
        trim = trim,
      )
    }

  def executeSyncStreamErr(
      stdIn: CommandInputSource = CommandInputSource.Empty,
      stdErr: CommandOutputSource = CommandOutputSource.PipeStdErr,
      trim: Boolean = true,
  )(using Trace): IO[CommandError, (stdOut: String, exitCode: Int)] =
    CommandService {
      _.executeSyncStreamErr(
        command = build,
        stdIn = stdIn,
        stdErr = stdErr,
        trim = trim,
      )
    }

  def executeCode(
      stdIn: CommandInputSource = CommandInputSource.Empty,
      stdOut: CommandOutputSource = CommandOutputSource.PipeStdOut,
      stdErr: CommandOutputSource = CommandOutputSource.PipeStdErr,
  )(using Trace): IO[CommandError, Int] =
    CommandService {
      _.executeCode(
        command = build,
        stdIn = stdIn,
        stdOut = stdOut,
        stdErr = stdErr,
      )
    }

  def executeCodeSuccess(
      stdIn: CommandInputSource = CommandInputSource.Empty,
      stdOut: CommandOutputSource = CommandOutputSource.PipeStdOut,
      stdErr: CommandOutputSource = CommandOutputSource.PipeStdErr,
  )(using Trace): IO[CommandError, Unit] =
    CommandService {
      _.executeCodeSuccess(
        command = build,
        stdIn = stdIn,
        stdOut = stdOut,
        stdErr = stdErr,
      )
    }

  def executeSyncDecodeWith[A](
      stdIn: CommandInputSource = CommandInputSource.Empty,
      stdErrOnSuccess: CommandOutputSource = CommandOutputSource.Log(LogLevels.Detailed, ShowCommand.FullCommand),
      trim: Boolean = true,
  )(
      decode: String => Either[Error, A],
  )(using Trace): IO[CommandError, A] =
    CommandService {
      _.executeSyncDecodeWith(command = build, stdIn = stdIn, stdErrOnSuccess = stdErrOnSuccess, trim = trim) { decode }
    }

  def executeSyncDecodeString[A: StringDecoder as dec](
      stdIn: CommandInputSource = CommandInputSource.Empty,
      stdErrOnSuccess: CommandOutputSource = CommandOutputSource.Log(LogLevels.Detailed, ShowCommand.FullCommand),
      trim: Boolean = true,
  )(using Trace): IO[CommandError, A] =
    CommandService {
      _.executeSyncDecodeWith(command = build, stdIn = stdIn, stdErrOnSuccess = stdErrOnSuccess, trim = trim) { dec.decodeError }
    }

  def executeSyncDecodePlainText[A: PlainTextSchema as schema](
      stdIn: CommandInputSource = CommandInputSource.Empty,
      stdErrOnSuccess: CommandOutputSource = CommandOutputSource.Log(LogLevels.Detailed, ShowCommand.FullCommand),
      trim: Boolean = true,
  )(using Trace): IO[CommandError, A] =
    CommandService {
      _.executeSyncDecodeWith(command = build, stdIn = stdIn, stdErrOnSuccess = stdErrOnSuccess, trim = trim) { schema.decode(_).leftMap { Error(_) } }
    }

  def executeSyncDecodeJson[A: JsonDecoder as dec](
      stdIn: CommandInputSource = CommandInputSource.Empty,
      stdErrOnSuccess: CommandOutputSource = CommandOutputSource.Log(LogLevels.Detailed, ShowCommand.FullCommand),
      trim: Boolean = true,
  )(using Trace): IO[CommandError, A] =
    CommandService {
      _.executeSyncDecodeWith(command = build, stdIn = stdIn, stdErrOnSuccess = stdErrOnSuccess, trim = trim) { dec.decodeJsonString }
    }

  /////// Convenience methods ///////////////////////////////////////////////////////////////

  // Higher-level wrappers over the `execute*` methods above: run-and-log, assert a zero exit code, capture stdout.

  def execute(
      outLevel: LogLevel = LogLevel.Info,
      errorLevel: LogLevel = LogLevel.Error,
      annotateCommand: Boolean = true,
  )(using trace: Trace): Task[Int] =
    CommandService {
      _.executeCode(
        command = build,
        stdIn = CommandInputSource.Empty,
        stdOut = CommandOutputSource.Log(outLevel, if annotateCommand then ShowCommand.CommandName else ShowCommand.Empty),
        stdErr = CommandOutputSource.Log(errorLevel, if annotateCommand then ShowCommand.CommandName else ShowCommand.Empty),
      )
    }

  def executeSuccess(
      outLevel: LogLevel = LogLevel.Info,
      errorLevel: LogLevel = LogLevel.Error,
      annotateCommand: Boolean = true,
  )(using trace: Trace): IO[CommandError, Unit] =
    CommandService {
      _.executeCodeSuccess(
        command = build,
        stdIn = CommandInputSource.Empty,
        stdOut = CommandOutputSource.Log(outLevel, if annotateCommand then ShowCommand.CommandName else ShowCommand.Empty),
        stdErr = CommandOutputSource.Log(errorLevel, if annotateCommand then ShowCommand.CommandName else ShowCommand.Empty),
      )
    }

  def executeString(
      errorLevel: LogLevel = LogLevel.Error,
      annotateCommand: Boolean = true,
  )(using trace: Trace): IO[CommandError, String] =
    CommandService {
      _.executeStringSuccess(
        command = build,
        stdIn = CommandInputSource.Empty,
        stdErr = CommandOutputSource.Log(errorLevel, if annotateCommand then ShowCommand.CommandName else ShowCommand.Empty),
        trim = true,
      )
    }

  def executeNoLogger: IO[CommandError, Int] =
    executeCode(stdIn = CommandInputSource.Empty)

  def executeNoLoggerSuccess: Task[Unit] =
    executeCodeSuccess(stdIn = CommandInputSource.Empty)

}
object Command {

  def apply(command: String): Command = new Command(false, command, Growable.Empty, None, Growable.empty)

  final case class Args(args: Growable[String])
  object Args {

    trait ToArgs[-A] {
      def toArgs(a: A): Args
    }
    object ToArgs {

      given id: ToArgs[Args] =
        identity(_)

      given string: ToArgs[String] =
        str => Args(Growable.single(str))

      given option: [A] => (aToArgs: ToArgs[A]) => ToArgs[Option[A]] = {
        case Some(a) => aToArgs.toArgs(a)
        case None    => Args(Growable.empty)
      }

      given seq: [S[_], A] => (seqOps: SeqOps[S], aToArgs: ToArgs[A]) => ToArgs[S[A]] =
        sa => Args(Growable.many(sa).flatMap(aToArgs.toArgs(_).args))

    }

    given convertToArgs: [A] => (aToArgs: ToArgs[A]) => Conversion[A, Args] =
      aToArgs.toArgs(_)

  }

}
