package oxygen.zio.system

import oxygen.predef.core.*
import scala.sys.process.*
import zio.*

final class Command private (isSudo: Boolean, command: String, args: Growable[String], file: Option[java.io.File | java.nio.file.Path], env: Growable[(String, String)]) {

  lazy val fullCommand: Growable[String] =
    if isSudo then "sudo" +: command +: args
    else command +: args

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def sudo: Command = sudoIf(true)
  def sudoIf(cond: Boolean): Command = new Command(cond, command, args, file, env)

  def apply(args: Command.Args*): Command =
    new Command(isSudo, command, this.args ++ Growable.many(args).flatMap(_.args), file, env)

  def addEnv(env: Growable[(String, String)]): Command = new Command(isSudo, command, args, file, this.env ++ env)
  def envVar(key: String, value: String): Command = new Command(isSudo, command, args, file, this.env :+ (key, value))
  def envVar(key: String, value: Option[String]): Command = value.fold(this)(this.envVar(key, _))

  def cwd(file: java.io.File): Command = new Command(isSudo, command, args, file.some, env)
  def cwd(file: Option[java.io.File]): Command = new Command(isSudo, command, args, file, env)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Execute
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def toProcess: ProcessBuilder = Process(fullCommand.to[Seq], None, env.to[Seq]*)

  def execute(
      outLevel: LogLevel = LogLevel.Info,
      errorLevel: LogLevel = LogLevel.Error,
  )(using trace: Trace): Task[Int] =
    (for {
      logger <- ZProcessLogger.make(outLevel = outLevel, errorLevel = errorLevel)
      code <- ZIO.attempt { toProcess.!(logger) }
    } yield code) @@ ZIOAspect.annotated("command", command)

  def executeSuccess(
      outLevel: LogLevel = LogLevel.Info,
      errorLevel: LogLevel = LogLevel.Error,
  )(using trace: Trace): Task[Unit] =
    execute(outLevel = outLevel, errorLevel = errorLevel).flatMap {
      case 0    => ZIO.unit
      case code => ZIO.fail(new RuntimeException(s"Command '$command' exited with non-zero code: $code"))
    }

  def executeString(
      errorLevel: LogLevel = LogLevel.Error,
  )(using trace: Trace): Task[String] =
    (for {
      logger <- ZProcessLogger.make(errorLevel = errorLevel)
      output <- ZIO.attempt { toProcess.!!(logger) }
    } yield output) @@ ZIOAspect.annotated("command", command)

  def executeNoLogger: Task[Int] = ZIO.attempt { toProcess.! }

  def executeNoLoggerSuccess: Task[Unit] =
    executeNoLogger.flatMap {
      case 0    => ZIO.unit
      case code => ZIO.fail(new RuntimeException(s"Command '$command' exited with non-zero code: $code"))
    }

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
