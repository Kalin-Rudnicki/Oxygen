package oxygen.zio.system

import java.nio.file as J
import java.time.Instant
import oxygen.predef.core.*
import oxygen.zio.error.FileSystemError
import oxygen.zio.syntax.error.*
import zio.*
import zio.stream.*

final case class JavaPath(javaPath: J.Path) extends Path {

  private lazy val absoluteJavaPath: J.Path = javaPath.toAbsolutePath

  override lazy val pathName: Path.PathName = Path.PathName(javaPath.toString)
  override lazy val fileName: Path.FileName = Path.FileName.of(javaPath.getFileName.toString)

  /////// Conversion ///////////////////////////////////////////////////////////////

  override def toJavaFile: IO[FileSystemError, java.io.File] = FileSystem.attempt(pathName, "convert path to java file") { javaPath.toFile }
  override def toJavaPath: IO[FileSystemError, java.nio.file.Path] = ZIO.succeed(javaPath)

  override def real: IO[FileSystemError, Path] = FileSystem.attempt(pathName, "resolve 'real' path") { JavaPath(javaPath.toRealPath()) }

  override def absolute: Path = JavaPath(absoluteJavaPath)

  override def normalized: Path = JavaPath(javaPath.normalize)

  override def rootOption: Option[Path] = Option(javaPath.getRoot).map(JavaPath(_))

  override def parentOption: Option[Path] = Option(javaPath.getParent).map(JavaPath(_))

  override def resolve(other: String): Path = JavaPath(javaPath.resolve(other))

  override def relativize(that: Path): Path = {
    val _that: JavaPath = JavaPath.unsafeJava(that)
    JavaPath(this.javaPath.relativize(_that.javaPath))
  }

  override def isSame(that: Path): Boolean =
    try {
      val _that: JavaPath = JavaPath.unsafeJava(that)
      J.Files.isSameFile(this.javaPath, _that.javaPath)
    } catch {
      case _ => false
    }

  override def isParentOf(that: Path): Boolean =
    try {
      val _that: JavaPath = JavaPath.unsafeJava(that)
      _that.absoluteJavaPath.startsWith(this.absoluteJavaPath)
    } catch {
      case _ => false
    }

  override def isChildOf(that: Path): Boolean =
    that.isParentOf(this)

  /////// Read ///////////////////////////////////////////////////////////////

  override def read: IO[FileSystemError, String] =
    FileSystem.attempt(pathName, "read from path") { J.Files.readString(javaPath) }

  override def status: IO[FileSystemError, Path.Status] =
    FileSystem.attempt(pathName, "detect path status") {
      if !J.Files.exists(javaPath) then Path.Status.DoesNotExist
      else if J.Files.isRegularFile(javaPath) then Path.Type.File
      else if J.Files.isDirectory(javaPath) then Path.Type.Directory
      else Path.Type.Other // TODO (KR) :
    }

  override def exists: IO[FileSystemError, Boolean] = FileSystem.attempt(pathName, "detect file existence") { J.Files.exists(javaPath) }

  override def notExists: IO[FileSystemError, Boolean] = FileSystem.attempt(pathName, "detect file existence") { J.Files.notExists(javaPath) }

  override def lastModifiedAt: IO[FileSystemError, Instant] = FileSystem.attempt(pathName, "read file timestamp") { J.Files.getLastModifiedTime(javaPath).toInstant }

  override def childStream: Stream[FileSystemError, Path] =
    for {
      javaStream <- ZStream.scoped { FileSystem.attempt(pathName, "get directory children") { J.Files.list(javaPath) }.withFinalizer { s => ZIO.succeed { s.close() } } }
      childJavaPath <- ZStream.fromJavaIterator { javaStream.iterator() }.convertCausesFail(FileSystemError.GenericOperationError(pathName, "traverse java path iterator", _))
    } yield JavaPath(childJavaPath)

  override def isHidden: IO[FileSystemError, Boolean] = FileSystem.attempt(pathName, "get path hidden property") { J.Files.isHidden(javaPath) }

  override def isReadable: IO[FileSystemError, Boolean] = FileSystem.attempt(pathName, "get path read property") { J.Files.isReadable(javaPath) }

  override def isWritable: IO[FileSystemError, Boolean] = FileSystem.attempt(pathName, "get path write property") { J.Files.isWritable(javaPath) }

  override def isExecutable: IO[FileSystemError, Boolean] = FileSystem.attempt(pathName, "get path executable property") { J.Files.isExecutable(javaPath) }

  /////// Write ///////////////////////////////////////////////////////////////

  override def write(contents: String): IO[FileSystemError, Unit] =
    FileSystem.attempt(pathName, "write to path") { J.Files.writeString(javaPath, contents) }

  override def createEmptyFile: IO[FileSystemError, Unit] = FileSystem.attempt(pathName, "create empty file") { J.Files.createFile(javaPath) }

  override def createDirectory: IO[FileSystemError, Unit] = FileSystem.attempt(pathName, "create directory") { J.Files.createDirectory(javaPath) }

  override def createDirectories: IO[FileSystemError, Unit] = FileSystem.attempt(pathName, "create directories") { J.Files.createDirectories(javaPath) }

  override def copyTo(destination: Path): IO[FileSystemError, Unit] =
    JavaPath.safeJava(destination).flatMap { destination =>
      FileSystem.attempt(pathName, s"copy to $destination") { J.Files.copy(this.javaPath, destination.javaPath) }
    }

  override def moveTo(destination: Path): IO[FileSystemError, Unit] =
    JavaPath.safeJava(destination).flatMap { destination =>
      FileSystem.attempt(pathName, s"move to $destination") { J.Files.move(this.javaPath, destination.javaPath) }
    }

  /////// Delete ///////////////////////////////////////////////////////////////

  override def delete: IO[FileSystemError, Unit] = FileSystem.attempt(pathName, "delete file") { J.Files.delete(javaPath) }

  override def deleteIfExists: IO[FileSystemError, Unit] = FileSystem.attempt(pathName, "delete file") { J.Files.deleteIfExists(javaPath) }

}
object JavaPath {

  private def unsafeJava(path: Path): JavaPath = path match
    case path: JavaPath => path
    case _              => throw Error(str"Not a java path: (${path.getClass.getName}) ${path.pathName}")

  private def safeJava(path: Path): UIO[JavaPath] = path match
    case path: JavaPath => ZIO.succeed(path)
    case _              => ZIO.die(Error(str"Not a java path: (${path.getClass.getName}) ${path.pathName}"))

}
