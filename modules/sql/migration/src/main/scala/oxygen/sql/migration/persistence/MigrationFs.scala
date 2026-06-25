package oxygen.sql.migration.persistence

import oxygen.core.Version
import oxygen.predef.json.*
import oxygen.sql.migration.persistence.model.PersistedMigrationFile
import oxygen.zio.error.FileSystemError
import oxygen.zio.system.Path
import scala.collection.immutable.ArraySeq
import zio.*

/**
  * Reads and writes the filesystem migration history -- one [[PersistedMigrationFile]] per version,
  * named `<version>.json` (e.g. `1.2.0.json`).
  *
  * The version files are authoritative: there is no `current.json` (it would be derivable from the
  * highest version file's snapshot, and could go stale). The diff source for generation is simply
  * the highest version file's `state` (see [[latestSnapshot]]).
  */
final case class MigrationFs(dir: Path) {

  import MigrationFs.*

  /** All migration files in the directory, ordered ascending by version. Empty if the dir is absent. */
  def list: IO[MigrationFsError, ArraySeq[Entry]] =
    dir.exists.mapError(MigrationFsError.FileSystemFailure(_)).flatMap {
      case false => ZIO.succeed(ArraySeq.empty)
      case true  =>
        for {
          children <- dir.children.mapError(MigrationFsError.FileSystemFailure(_))
          jsonFiles = children.filter(_.fileName.hasExtension(fileExtension))
          entries <- ZIO.foreach(jsonFiles)(parseEntry)
        } yield ArraySeq.from(entries).sortBy(_.version)
    }

  /** The highest-versioned migration file, or `None` if there are none. */
  def latest: IO[MigrationFsError, Option[Entry]] =
    list.map(_.lastOption)

  /** The resulting state snapshot of the highest-versioned migration -- the diff source for generation. */
  def latestSnapshot: IO[MigrationFsError, Option[(Version, PersistedMigrationFile)]] =
    latest.map(_.map(e => (e.version, e.file)))

  /** Writes a migration file as `<version>.json`, creating the directory if needed. */
  def write(file: PersistedMigrationFile): IO[MigrationFsError, Path] =
    for {
      _ <- ZIO.fromEither(parseVersion(file.version)).mapError(MigrationFsError.InvalidVersion(file.version, _))
      _ <- dir.createDirectories.mapError(MigrationFsError.FileSystemFailure(_))
      path = dir.resolve(s"${file.version}.$fileExtension")
      _ <- path.write(file.toJsonStringPretty).mapError(MigrationFsError.FileSystemFailure(_))
    } yield path

  private def parseEntry(path: Path): IO[MigrationFsError, Entry] =
    for {
      version <- ZIO.fromEither(parseVersion(path.fileName.baseName)).mapError(MigrationFsError.InvalidFileName(path.fileName.name, _))
      contents <- path.read.mapError(MigrationFsError.FileSystemFailure(_))
      file <- ZIO
        .fromEither(contents.fromJsonString[PersistedMigrationFile])
        .mapError(e => MigrationFsError.DecodeFailure(path.fileName.name, e.getMessage))
      declared <- ZIO.fromEither(parseVersion(file.version)).mapError(MigrationFsError.InvalidVersion(file.version, _))
      _ <- ZIO.unless(declared == version)(ZIO.fail(MigrationFsError.VersionMismatch(path.fileName.name, file.version)))
    } yield Entry(version, path, file)

}
object MigrationFs {

  val fileExtension: String = "json"

  final case class Entry(
      version: Version,
      path: Path,
      file: PersistedMigrationFile,
  )

  private def parseVersion(string: String): Either[String, Version] =
    Version.parse(string).toRight(s"invalid version: $string")

  enum MigrationFsError {
    case FileSystemFailure(cause: FileSystemError)
    case InvalidFileName(fileName: String, reason: String)
    case InvalidVersion(version: String, reason: String)
    case DecodeFailure(fileName: String, reason: String)
    case VersionMismatch(fileName: String, declaredVersion: String)
  }

}
