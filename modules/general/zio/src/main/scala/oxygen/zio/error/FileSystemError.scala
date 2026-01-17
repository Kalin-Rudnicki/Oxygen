package oxygen.zio.error

import oxygen.predef.core.*
import oxygen.zio.ZioCauses
import oxygen.zio.system.Path.{PathName, Type as FileType}

sealed trait FileSystemError extends Error
object FileSystemError {

  final case class PathDoesNotExist(path: PathName) extends FileSystemError {
    override def errorMessage: Text = str"Path does not exist: $path"
  }

  final case class PathAlreadyExists(path: PathName) extends FileSystemError {
    override def errorMessage: Text = str"Path already exists: $path"
  }

  final case class InvalidFileType(path: PathName, expected: FileType, actual: FileType) extends FileSystemError {
    override def errorMessage: Text = str"Path ($path) references an invalid file type [expected=${expected.toText}, actual=${actual.toText}]"
  }

  final case class Unimplemented(path: PathName, operation: String) extends FileSystemError {
    override def errorMessage: Text = str"Unsupported file-system operation ($operation) [path=$path]"
  }

  final case class GenericOperationError(path: PathName, whileAttemptingTo: String, override val causes: ZioCauses) extends FileSystemError {
    override def errorMessage: Text = str"File-system error while attempting to $whileAttemptingTo: $path"
  }

}
