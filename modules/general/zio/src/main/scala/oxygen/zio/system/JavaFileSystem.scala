package oxygen.zio.system

import java.nio.file as J
import oxygen.predef.core.*
import oxygen.zio.error.FileSystemError
import zio.*

object JavaFileSystem extends FileSystem {

  override def path(p: String): IO[FileSystemError, Path] =
    FileSystem.attempt(Path.PathName(p), "instantiate path") { JavaPath(J.Path.of(p)) }

  override def path(p: NonEmptyList[String]): IO[FileSystemError, Path] =
    FileSystem.attempt(Path.PathName(p.mkString("/")), "instantiate path") { JavaPath(J.Path.of(p.head, p.tail*)) }

}
