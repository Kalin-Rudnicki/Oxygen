package oxygen.zio.system

import oxygen.predef.core.*
import oxygen.zio.ZIOAspectPoly
import zio.*

// FIX-PRE-MERGE (KR) : do this?

sealed trait CommandInputSource {

  final def toNonEmpty: Option[CommandInputSource.NonEmpty] = this match
    case CommandInputSource.Empty              => None
    case nonEmpty: CommandInputSource.NonEmpty => nonEmpty.some

}
object CommandInputSource {

  case object Empty extends CommandInputSource

  sealed trait NonEmpty extends CommandInputSource
  case object Pipe extends CommandInputSource.NonEmpty
  final case class File(path: Path) extends CommandInputSource.NonEmpty
  final case class Const(stdIn: String) extends CommandInputSource.NonEmpty
  final case class Stream(stdIn: zio.stream.Stream[Throwable, Byte]) extends CommandInputSource.NonEmpty

}

sealed trait CommandOutputSource {

  final def toNonEmpty: Option[CommandOutputSource.NonEmpty] = this match
    case CommandOutputSource.Empty              => None
    case nonEmpty: CommandOutputSource.NonEmpty => nonEmpty.some

}
object CommandOutputSource {

  case object Empty extends CommandOutputSource

  sealed trait NonEmpty extends CommandOutputSource
  case object PipeStdOut extends CommandOutputSource.NonEmpty
  case object PipeStdErr extends CommandOutputSource.NonEmpty
  final case class File(path: Path) extends CommandOutputSource.NonEmpty
  final case class Log(logLevel: LogLevel, showCommand: ShowCommand) extends CommandOutputSource.NonEmpty

  // TODO (KR) : have some way to pipe and collect? collect into StringBuilder? seems not worth it for the moment.

}

sealed trait ShowCommand {

  final def toNonEmpty: Option[ShowCommand.NonEmpty] = this match
    case ShowCommand.Empty              => None
    case nonEmpty: ShowCommand.NonEmpty => nonEmpty.some

}
object ShowCommand {

  case object Empty extends ShowCommand

  sealed trait NonEmpty extends ShowCommand {

    def show(cmd: BuiltCommand): String

    final def toAspect(cmd: BuiltCommand): ZIOAspectPoly = ZIOAspect.annotated("command", show(cmd))

  }

  case object CommandName extends ShowCommand.NonEmpty {
    override def show(cmd: BuiltCommand): String = cmd.commandIgnoreSudo
  }

  case object FullCommand extends ShowCommand.NonEmpty {
    override def show(cmd: BuiltCommand): String = cmd.showCommand(false).toString
  }

}
