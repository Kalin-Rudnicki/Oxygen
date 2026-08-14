package oxygen.executable.config

import oxygen.predef.core.*
import oxygen.zio.error.FileSystemError
import oxygen.zio.system.Path

/**
  * Typed errors raised by [[ConfigFileService]].
  *
  * Wraps lower-level [[FileSystemError]]s together with JSON/YAML decode failures, so callers get a
  * single, exhaustive error channel for the whole config-file lifecycle. Style mirrors
  * `TestContainerError` / `MigrationError`.
  */
sealed trait ConfigFileError extends Throwable {

  val path: Path.PathName

  override final def getMessage: String = this match
    case ConfigFileError.FileSystem(path, cause) =>
      s"File-system error for config file ($path): ${cause.safeGetMessage}"
    case ConfigFileError.UnsupportedExtension(path, extension) =>
      s"Unsupported config file extension${extension.fold("")(e => s" ($e)")} for ($path). Supported: ${ConfigFileError.supportedExtensions.mkString(", ")}"
    case ConfigFileError.JsonDecodeFailure(path, message) =>
      s"Unable to decode JSON config file ($path): $message"
    case ConfigFileError.YamlDecodeFailure(path, message) =>
      s"Unable to decode YAML config file ($path): $message"
    case ConfigFileError.EmptyDirectory(path) =>
      s"Config directory ($path) contains no valid config files (${ConfigFileError.supportedExtensions.mkString(", ")})"
    case ConfigFileError.NotFileOrDirectory(path) =>
      s"Config path ($path) is neither a file nor a directory"
    case ConfigFileError.PathDoesNotExist(path) =>
      s"Config path does not exist ($path)"

}
object ConfigFileError {

  val supportedExtensions: Set[String] = Set("json", "yaml", "yml")

  final case class FileSystem(path: Path.PathName, cause: FileSystemError) extends ConfigFileError
  final case class UnsupportedExtension(path: Path.PathName, extension: Option[String]) extends ConfigFileError
  final case class JsonDecodeFailure(path: Path.PathName, message: String) extends ConfigFileError
  final case class YamlDecodeFailure(path: Path.PathName, message: String) extends ConfigFileError
  final case class EmptyDirectory(path: Path.PathName) extends ConfigFileError
  final case class NotFileOrDirectory(path: Path.PathName) extends ConfigFileError
  final case class PathDoesNotExist(path: Path.PathName) extends ConfigFileError

  /** Lift a [[FileSystemError]] into the [[ConfigFileError]] channel. */
  def fromFileSystem(error: FileSystemError): ConfigFileError = ConfigFileError.FileSystem(error.path, error)

}
