package oxygen.zio.system

import scala.annotation.nowarn
import scala.sys.process.*
import zio.*

final class Command private (isSudo: Boolean, command: String, args: List[String]) {

  val cmd: List[String] =
    if (isSudo) "sudo" :: command :: args
    else command :: args

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def sudo: Command = sudoIf(true)
  def sudoIf(cond: Boolean): Command = new Command(cond, command, args)

  def ++(args: Seq[String]): Command = new Command(isSudo, command, this.args ++ args)

  def apply(args: Command.Arg*): Command =
    this ++ args.flatMap { case Command.Arg(args) => args }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Execute
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def execute(
      outLevel: LogLevel = LogLevel.Info,
      errorLevel: LogLevel = LogLevel.Error,
  )(using trace: Trace): Task[Int] =
    (for {
      logger <- ZProcessLogger.make(outLevel = outLevel, errorLevel = errorLevel)
      code <- ZIO.attempt { cmd.!(logger) }
    } yield code) @@ ZIOAspect.annotated("command", command)

  def executeSuccess(
      outLevel: LogLevel = LogLevel.Info,
      errorLevel: LogLevel = LogLevel.Error,
  )(using trace: Trace): Task[Unit] =
    execute(outLevel = outLevel, errorLevel = errorLevel).flatMap {
      case 0    => ZIO.unit
      case code => ZIO.fail(new RuntimeException(s"Command exited with non-zero code: $code"))
    }

  def executeString(
      errorLevel: LogLevel = LogLevel.Error,
  )(using trace: Trace): Task[String] =
    (for {
      logger <- ZProcessLogger.make(errorLevel = errorLevel)
      output <- ZIO.attempt { cmd.!!(logger) }
    } yield output) @@ ZIOAspect.annotated("command", command)

}
object Command {

  def apply(command: String): Command = new Command(false, command, Nil)

  type Arg = String | Option[String] | Seq[String] | Option[Seq[String]]
  object Arg {

    @nowarn
    def unapply(arg: Arg): Some[List[String]] = arg match
      case str: String            => Some(str :: Nil)
      case seq: Seq[String]       => Some(seq.toList)
      case None                   => Some(Nil)
      case Some(str: String)      => Some(str :: Nil)
      case Some(seq: Seq[String]) => Some(seq.toList)

  }

}
