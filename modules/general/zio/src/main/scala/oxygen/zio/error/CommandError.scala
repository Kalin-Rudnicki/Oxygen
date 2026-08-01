package oxygen.zio.error

import oxygen.predef.core.*
import oxygen.zio.system.*

sealed trait CommandError extends Error {

  val command: BuiltCommand

  protected final def failedToExecuteCommand: Text =
    str"Failed to execute command [ ${command.showCommand(false)} ]"

}
object CommandError {

  final case class ExecutionFailure(command: BuiltCommand, cause: Error) extends CommandError {
    override def errorMessage: Text = failedToExecuteCommand
    override def causes: ArraySeq[Error] = ArraySeq(cause)
  }

  final case class NonZeroExit(command: BuiltCommand, exit: Int, stdOut: Option[String], stdErr: Option[String]) extends CommandError {
    override def errorMessage: Text = str"$failedToExecuteCommand: Non-zero exit code ($exit)"
    override def causes: ArraySeq[Error] = ArraySeq.from(stdErr.map(Error(_)))
  }

  final case class DecodingFailure(command: BuiltCommand, error: Error, stdOut: Option[String], stdErr: Option[String]) extends CommandError {
    override def errorMessage: Text = str"$failedToExecuteCommand: Decoding failure"
    override def causes: ArraySeq[Error] = stdErr match
      case Some(stdErr) => ArraySeq(error, Error(stdErr))
      case None         => ArraySeq(error)
  }

  final case class Unimplemented(command: BuiltCommand) extends CommandError {
    override def errorMessage: Text = str"$failedToExecuteCommand: Operation not supported on this platform"
  }

}
