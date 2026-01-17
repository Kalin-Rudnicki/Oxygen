package oxygen.zio.system

import java.time.Instant
import oxygen.json.JsonDecoder
import oxygen.predef.core.*
import oxygen.zio.error.FileSystemError
import zio.*
import zio.stream.*

trait Path {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Abstract
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val pathName: Path.PathName
  lazy val fileName: Path.FileName

  /////// Conversion ///////////////////////////////////////////////////////////////

  def toJavaFile: IO[FileSystemError, java.io.File]
  def toJavaPath: IO[FileSystemError, java.nio.file.Path]

  def real: IO[FileSystemError, Path]

  def absolute: Path
  def normalized: Path
  def rootOption: Option[Path]
  def parentOption: Option[Path]

  def resolve(other: String): Path
  final def resolve(others: String*): Path = others.foldLeft(this)(_.resolve(_))

  def relativize(that: Path): Path

  def isSame(that: Path): Boolean
  def isParentOf(that: Path): Boolean
  def isChildOf(that: Path): Boolean

  /////// Read ///////////////////////////////////////////////////////////////

  def read: IO[FileSystemError, String]

  def status: IO[FileSystemError, Path.Status]

  def exists: IO[FileSystemError, Boolean]
  def notExists: IO[FileSystemError, Boolean]

  def lastModifiedAt: IO[FileSystemError, Instant]

  def childStream: Stream[FileSystemError, Path]

  def isHidden: IO[FileSystemError, Boolean]
  def isReadable: IO[FileSystemError, Boolean]
  def isWritable: IO[FileSystemError, Boolean]
  def isExecutable: IO[FileSystemError, Boolean]

  /////// Write ///////////////////////////////////////////////////////////////

  def write(contents: String): IO[FileSystemError, Unit]

  def createEmptyFile: IO[FileSystemError, Unit]
  def createDirectory: IO[FileSystemError, Unit]
  def createDirectories: IO[FileSystemError, Unit]

  def copyTo(destination: Path): IO[FileSystemError, Unit]
  def moveTo(destination: Path): IO[FileSystemError, Unit]

  /////// Delete ///////////////////////////////////////////////////////////////

  def delete: IO[FileSystemError, Unit]
  def deleteIfExists: IO[FileSystemError, Unit]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Concrete
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final def `type`: IO[FileSystemError, Path.Type] =
    this.status.flatMap {
      case tpe: Path.Type           => ZIO.succeed(tpe)
      case Path.Status.DoesNotExist => ZIO.fail(FileSystemError.PathDoesNotExist(pathName))
    }

  final def assertDoesNotExist: IO[FileSystemError, Unit] =
    ZIO.fail(FileSystemError.PathAlreadyExists(pathName)).whenZIODiscard(this.exists)

  final def assertDoesNotExistOr(expTpe: Path.Type): IO[FileSystemError, Unit] =
    this.status.flatMap {
      case Path.Status.DoesNotExist => ZIO.unit
      case `expTpe`                 => ZIO.unit
      case actTpe: Path.Type        => ZIO.fail(FileSystemError.InvalidFileType(pathName, expTpe, actTpe))
    }

  final def assertExists: IO[FileSystemError, Unit] =
    ZIO.fail(FileSystemError.PathDoesNotExist(pathName)).whenZIODiscard(this.notExists)

  final def assertExists(expTpe: Path.Type): IO[FileSystemError, Unit] =
    this.status.flatMap {
      case Path.Status.DoesNotExist => ZIO.fail(FileSystemError.PathDoesNotExist(pathName))
      case `expTpe`                 => ZIO.unit
      case actTpe: Path.Type        => ZIO.fail(FileSystemError.InvalidFileType(pathName, expTpe, actTpe))
    }

  final def children: IO[FileSystemError, Chunk[Path]] =
    childStream.runCollect

  final def recursiveFileStream(maxDepth: Int): Stream[FileSystemError, Path] =
    if maxDepth >= 0 then
      ZStream.fromZIO(status).flatMap {
        case Path.Status.DoesNotExist => ZStream.fail(FileSystemError.PathDoesNotExist(pathName))
        case Path.Type.File           => ZStream.succeed(this)
        case Path.Type.Directory      => this.childStream.flatMap(_.recursiveFileStream(maxDepth - 1))
        case Path.Type.Other          =>
          ZStream.fromZIO(this.real).flatMap { real =>
            ZStream.fromZIO(real.status).flatMap {
              case Path.Status.DoesNotExist => ZStream.fail(FileSystemError.PathDoesNotExist(real.pathName))
              case Path.Type.File           => ZStream.succeed(this)
              case Path.Type.Directory      => real.childStream.flatMap(_.recursiveFileStream(maxDepth - 1))
              case Path.Type.Other          => ZStream.logWarning(s"Unable to search 'other' file $this ($real)") *> ZStream.empty
            }
          }
      }
    else
      ZStream.empty

  final def recursiveFileStream(maxDepth: Int, extension: String): Stream[FileSystemError, Path] =
    recursiveFileStream(maxDepth).filter(_.fileName.hasExtension(extension))

  final def recursiveFileStream(maxDepth: Int, extensions: Set[String]): Stream[FileSystemError, Path] =
    recursiveFileStream(maxDepth).filter(_.fileName.hasExtension(extensions))

  final def readDecodeMessage[A](decode: String => Either[String, A]): IO[FileSystemError, A] =
    this.read.flatMap { contents =>
      decode(contents) match
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(FileSystemError.UnableToDecodeFileContents(pathName, Error.fromAny(error)))
    }

  final def readDecodeError[A](decode: String => Either[Error, A]): IO[FileSystemError, A] =
    this.read.flatMap { contents =>
      decode(contents) match
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(FileSystemError.UnableToDecodeFileContents(pathName, error))
    }

  final def readDecodeString[A: StringDecoder as dec]: IO[FileSystemError, A] =
    this.readDecodeMessage(dec.decodeDetailed)

  final def readDecodeJson[A: JsonDecoder as dec]: IO[FileSystemError, A] =
    this.readDecodeError(dec.decodeJsonString(_).leftMap(Error.fromThrowable))

  override final def toString: String = pathName.unwrap

}
object Path {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def of(path: String): IO[FileSystemError, Path] = FileSystem.current.get.flatMap(_.path(path))

  def of(path: NonEmptyList[String]): IO[FileSystemError, Path] = FileSystem.current.get.flatMap(_.path(path))

  def of(paths: List[String]): IO[FileSystemError, Path] = paths match
    case h :: t => Path.of(NonEmptyList(h, t))
    case Nil    => Path.of(".")

  def of(path: String*): IO[FileSystemError, Path] = Path.of(path.toList)

  def normalizedAbsolute(path: String): IO[FileSystemError, Path] = FileSystem.current.get.flatMap(_.path(path)).map(_.absolute.normalized)
  def currentWorkingDirectory: IO[FileSystemError, Path] = Path.normalizedAbsolute(".")

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Represents a Path.
    * path: a/b/c.d.e
    */
  opaque type PathName <: String = String
  object PathName {
    def apply(name: String): PathName = name
    extension (self: PathName)
      def unwrap: String = self
      def toPath: IO[FileSystemError, Path] = Path.of(self)
      def toNormalizedAbsolutePath: IO[FileSystemError, Path] = Path.normalizedAbsolute(self)
  }

  /**
    * path: a/b/c.d.e
    * [[full]] : "c.d.e"
    * [[baseName]] : "c.d"
    * [[extension]] : Some("e")
    *
    * path: a/b/c.d.e
    * [[full]] : "c.d.e"
    * [[baseName]] : "c.d"
    * [[extension]] : Some("e")
    */
  final case class FileName private (
      full: String,
      baseName: String,
      extension: Option[String],
  ) {

    def hasExtension(ext: String): Boolean = extension.contains(ext)
    def hasExtension(ext: Set[String]): Boolean = extension.exists(ext.contains)

    def name: String = full
    override def toString: String = full

  }
  object FileName {

    private val reg = "^(.*)\\.([^.]+)$".r

    def of(full: String): FileName = full match
      case _ if full.contains('/')  => throw Error("File names are not allowed to contain '/'")
      case reg(baseName, extension) => FileName(full, baseName, extension.some)
      case _                        => FileName(full, full, None)

    def baseAndExtension(baseName: String, extension: String): FileName =
      FileName(
        s"$baseName.$extension",
        baseName,
        extension.some,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Status
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Status
  object Status {
    case object DoesNotExist extends Status
  }

  sealed trait Type extends Status
  object Type {
    case object File extends Type
    case object Directory extends Type
    case object Other extends Type // TODO (KR) :
  }

}
