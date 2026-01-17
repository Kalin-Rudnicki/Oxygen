package oxygen.zio.system

import oxygen.predef.core.*
import oxygen.zio.error.FileSystemError
import zio.*

object UnimplementedFileSystem extends FileSystem {
  override def path(p: String): IO[FileSystemError, Path] = FileSystem.unimplementedDie(p, "create path")
  override def path(p: NonEmptyList[String]): IO[FileSystemError, Path] = path(p.mkString("/"))
}
