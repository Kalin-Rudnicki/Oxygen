package oxygen.zio.system

import oxygen.zio.error.CommandError
import zio.*

object UnimplementedCommandService extends CommandService {

  override def executeSync(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      trim: Boolean,
  )(using Trace): IO[CommandError, (stdOut: String, stdErr: String, exitCode: Int)] =
    ZIO.fail(CommandError.Unimplemented(command))

  override def executeSyncStreamErr(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdErr: CommandOutputSource,
      trim: Boolean,
  )(using Trace): IO[CommandError, (stdOut: String, exitCode: Int)] =
    ZIO.fail(CommandError.Unimplemented(command))

  override def executeCode(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdOut: CommandOutputSource,
      stdErr: CommandOutputSource,
  )(using Trace): IO[CommandError, Int] =
    ZIO.fail(CommandError.Unimplemented(command))

}
