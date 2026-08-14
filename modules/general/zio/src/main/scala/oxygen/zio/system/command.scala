package oxygen.zio.system

import oxygen.json.JsonDecoder
import oxygen.predef.core.*
import oxygen.schema.*
import oxygen.zio.error.CommandError
import oxygen.zio.logging.LogLevels
import zio.*

final case class BuiltCommand(
    command: String,
    args: List[String],
    cwd: Option[Path],
    env: Map[String, String],
) extends Showable {

  def commandIgnoreSudo: String = (command, args) match
    case ("sudo", cmd :: _) => cmd
    case _                  => command

  def showCommand: Text = showCommand(false)
  def showCommand(forceEscape: Boolean): Text =
    Text.foreachJoined(command :: args, " ") { s => Text.fromString(BuiltCommand.safeShow(s, forceEscape)) }

  override def show: Text = showCommand

}
object BuiltCommand {

  given Conversion[Command2, BuiltCommand] = _.build

  /**
    * Render a single command token for _display / logging only_ (commands are executed via an explicit
    * argv list through `ProcessBuilder`, never through a shell). When a token needs quoting it is wrapped
    * in POSIX single quotes, with embedded single quotes escaped using the standard `'\''` idiom so that
    * the rendered string could be safely pasted into a shell.
    */
  def safeShow(value: String, forceEscape: Boolean): String = {
    val needsEscape: Boolean =
      forceEscape || value.isEmpty || value.exists {
        case '\'' | '"' | ' ' | '\t' | '\n' => true
        case _                              => false
      }

    if needsEscape then s"'${value.replace("'", "'\\''")}'"
    else value
  }

}

/**
  * Immutable builder for an external OS command, executed through [[CommandService]].
  *
  * This is the "v2" command API: unlike the legacy [[Command]] (which shells out via `scala.sys.process`),
  * execution goes through the pluggable [[CommandService]] with first-class stdin/stdout/stderr sources and
  * typed decoding of process output. It is intentionally kept alongside the legacy [[Command]] until all
  * call-sites have migrated; the `Command2` name is a deliberate interim so the two can coexist.
  */
final class Command2 private (isSudo: Boolean, command: String, args: Growable[String], cwdPath: Option[Path], env: Growable[(String, String)]) {

  lazy val fullCommand: Growable[String] =
    if isSudo then "sudo" +: command +: args
    else command +: args

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def sudo: Command2 = sudoIf(true)
  def sudoIf(cond: Boolean): Command2 = new Command2(cond, command, args, cwdPath, env)

  def apply(args: Command2.Args*): Command2 =
    new Command2(isSudo, command, this.args ++ Growable.many(args).flatMap(_.args), cwdPath, env)

  def addEnv(env: Growable[(String, String)]): Command2 = new Command2(isSudo, command, args, cwdPath, this.env ++ env)
  def addEnv(env: (String, String)*): Command2 = new Command2(isSudo, command, args, cwdPath, this.env ++ Growable.many(env))
  def envVar(key: String, value: String): Command2 = new Command2(isSudo, command, args, cwdPath, this.env :+ (key, value))
  def envVar(key: String, value: Option[String]): Command2 = value.fold(this)(this.envVar(key, _))

  def cwd(file: Path): Command2 = new Command2(isSudo, command, args, file.some, env)
  def cwd(file: Option[Path]): Command2 = new Command2(isSudo, command, args, file, env)

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

  /////// Legacy-compatible convenience methods ///////////////////////////////////////////////////////////////

  // These mirror the method names/shape of the legacy `Command` API to ease migration to `Command2`.
  // They are thin wrappers over the `execute*` methods above and can be retired once migration is complete.

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
object Command2 {

  def apply(command: String): Command2 = new Command2(false, command, Growable.Empty, None, Growable.empty)

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
