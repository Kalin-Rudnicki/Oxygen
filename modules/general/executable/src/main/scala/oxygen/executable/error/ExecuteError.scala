package oxygen.executable.error

import oxygen.cli.*
import oxygen.cli.error.*
import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.throwable.*
import oxygen.executable.{Executable, ExecutableApp}

sealed trait ExecuteError extends Throwable {

  override final def toString: String = getMessage

  override final def getMessage: String =
    this match {
      case ExecuteError.SourceError(source, cause)                          => s"Error parsing source $source : $cause"
      case ExecuteError.Parsing.FailedToBuild(error)                        => s"Failed to build program parser:\n$error"
      case ExecuteError.Parsing.FailedToParse(error, help)                  => s"Failed to parse:\n$error\n\nHelp:\n$help"
      case ExecuteError.Parsing.Help(help, _)                               => help.toString
      case ExecuteError.SubCommandError.MissingSubCommand(options)          => s"Missing sub-command, options: ${options.map(_._1).mkString(", ")}"
      case ExecuteError.SubCommandError.InvalidSubCommand(command, options) => s"Invalid sub-command '$command', options: ${options.map(_._1).mkString(", ")}"
      case ExecuteError.InvalidConfig(message)                              => s"Invalid config: $message"
      case ExecuteError.ProgramError(error)                                 =>
        error.asInstanceOf[Matchable] match
          case error: Throwable => error.safeGetMessage
          case _                => String.valueOf(error)
      case ExecuteError.Generic(operation, cause) => s"failed $operation: ${cause.safeGetMessage}"
    }

}
object ExecuteError {

  final case class SourceError(source: ExecutableApp.Config.Source, cause: SourceError.Cause) extends ExecuteError
  object SourceError {

    enum Cause {

      case InvalidJson(string: String, error: String)
      case SourceDNE
      case Generic(cause: Throwable)

      override final def toString: String = this match
        case Cause.InvalidJson(_, error) => s"Invalid json - $error"
        case Cause.SourceDNE             => "Source does not exist"
        case Cause.Generic(cause)        => cause.safeGetMessage

    }

  }

  sealed trait Parsing extends ExecuteError
  object Parsing {
    final case class FailedToBuild(error: BuildError) extends ExecuteError.Parsing
    final case class FailedToParse(error: ParseError, help: HelpMessage) extends ExecuteError.Parsing
    final case class Help(help: HelpMessage, helpType: HelpType) extends ExecuteError.Parsing
  }

  sealed trait SubCommandError extends ExecuteError
  object SubCommandError {
    final case class MissingSubCommand(options: NonEmptyList[(String, Executable)]) extends SubCommandError
    final case class InvalidSubCommand(command: String, options: NonEmptyList[(String, Executable)]) extends SubCommandError
  }

  final case class InvalidConfig(message: String) extends ExecuteError

  final case class ProgramError(error: Any) extends ExecuteError

  final case class Generic(operation: String, cause: Throwable) extends ExecuteError

}
