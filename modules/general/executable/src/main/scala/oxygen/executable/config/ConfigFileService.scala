package oxygen.executable.config

import oxygen.core.PlatformCompat
import oxygen.json.{Json, JsonDecoder, JsonEncoder}
import oxygen.predef.core.*
import oxygen.yaml.{YamlParser, YamlWriter}
import oxygen.zio.error.FileSystemError
import oxygen.zio.system.Path
import zio.*

/**
  * Reusable, injectable service that centralizes the config-file lifecycle: load / merge / validate
  * / persist JSON + YAML config files (e.g. `./.my-cli/local.json`, `~/.my-cli/global.json`).
  *
  * All path operations go through [[Path]] / `FileSystem.current`, so the service is testable by
  * swapping the current file-system or pointing at a temp directory.
  *
  * Supported extensions: `.json`, `.yaml`, `.yml`.
  *
  * Contrast with `@envConfig` (see `ConfigLoader`): `@envConfig` resolves a single env var to a
  * file/directory ONCE at application startup, whereas this service is the runtime API for reading,
  * writing, listing and merging config files while the app is running. Both share the same core
  * loading/merging logic so their behavior can never diverge.
  */
trait ConfigFileService {

  /** True iff `file` exists (any type). */
  def exists(file: Path): IO[ConfigFileError, Boolean]

  /** List the supported config files (`.json`/`.yaml`/`.yml`) directly inside `dir`, sorted by path. */
  def list(dir: Path): IO[ConfigFileError, Chunk[Path]]

  /** Read + parse a single config `file` (dispatch on extension) into a raw [[Json]]. */
  def loadJson(file: Path): IO[ConfigFileError, Json]

  /** Read + parse + decode a single config `file` (dispatch on extension) into `A`. */
  def load[A: JsonDecoder](file: Path): IO[ConfigFileError, A]

  /** Merge every supported config file in `dir` into a single [[Json]] via `reduceLeft(_ ++ _)`. */
  def mergeDirectoryJson(dir: Path): IO[ConfigFileError, Json]

  /** Merge every supported config file in `dir` and decode the result into `A`. */
  def mergeDirectory[A: JsonDecoder](dir: Path): IO[ConfigFileError, A]

  /** Resolve `path`: if a file -> [[loadJson]]; if a directory -> [[mergeDirectoryJson]]. */
  def loadResolvedJson(path: Path): IO[ConfigFileError, Json]

  /** Resolve `path` (file or directory) and decode the result into `A`. This is the `@envConfig` semantics. */
  def loadResolved[A: JsonDecoder](path: Path): IO[ConfigFileError, A]

  /**
    * Atomically persist `value` to `file`, serialized by its extension (JSON pretty / YAML).
    * Writes to a sibling temp file then moves it into place, so readers never observe a partial file.
    * Parent directories are created if missing.
    */
  def save[A: JsonEncoder](file: Path, value: A): IO[ConfigFileError, Unit]

}
object ConfigFileService {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Layers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** The real implementation. Stateless: behavior is entirely a function of `FileSystem.current`. */
  val live: ULayer[ConfigFileService] = ZLayer.succeed(Live)

  /** Alias for [[live]] — the default wiring. */
  val default: ULayer[ConfigFileService] = live

  /**
    * Alias for [[live]] — there is nothing to stub. Point `FileSystem.current` at a temp directory
    * (or a test file-system) to exercise the service in tests.
    */
  val test: ULayer[ConfigFileService] = live

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Accessors
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def exists(file: Path): ZIO[ConfigFileService, ConfigFileError, Boolean] = ZIO.serviceWithZIO(_.exists(file))
  def list(dir: Path): ZIO[ConfigFileService, ConfigFileError, Chunk[Path]] = ZIO.serviceWithZIO(_.list(dir))
  def loadJson(file: Path): ZIO[ConfigFileService, ConfigFileError, Json] = ZIO.serviceWithZIO(_.loadJson(file))
  def load[A: JsonDecoder](file: Path): ZIO[ConfigFileService, ConfigFileError, A] = ZIO.serviceWithZIO(_.load[A](file))
  def mergeDirectoryJson(dir: Path): ZIO[ConfigFileService, ConfigFileError, Json] = ZIO.serviceWithZIO(_.mergeDirectoryJson(dir))
  def mergeDirectory[A: JsonDecoder](dir: Path): ZIO[ConfigFileService, ConfigFileError, A] = ZIO.serviceWithZIO(_.mergeDirectory[A](dir))
  def loadResolvedJson(path: Path): ZIO[ConfigFileService, ConfigFileError, Json] = ZIO.serviceWithZIO(_.loadResolvedJson(path))
  def loadResolved[A: JsonDecoder](path: Path): ZIO[ConfigFileService, ConfigFileError, A] = ZIO.serviceWithZIO(_.loadResolved[A](path))
  def save[A: JsonEncoder](file: Path, value: A): ZIO[ConfigFileService, ConfigFileError, Unit] = ZIO.serviceWithZIO(_.save[A](file, value))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Live
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Live extends ConfigFileService {

    private def wrapFs[A](effect: IO[FileSystemError, A]): IO[ConfigFileError, A] =
      effect.mapError(ConfigFileError.fromFileSystem)

    override def exists(file: Path): IO[ConfigFileError, Boolean] =
      wrapFs(file.exists)

    override def list(dir: Path): IO[ConfigFileError, Chunk[Path]] =
      wrapFs(dir.children).map { children =>
        children
          .filter(_.fileName.hasExtension(ConfigFileError.supportedExtensions))
          .sortBy(_.pathName.unwrap)
      }

    override def loadJson(file: Path): IO[ConfigFileError, Json] =
      file.fileName.extension match
        case Some("json") =>
          wrapFs(file.read).flatMap { str =>
            ZIO.fromEither(JsonDecoder.json.decodeJsonString(str)).mapError { e => ConfigFileError.JsonDecodeFailure(file.pathName, e.safeGetMessage) }
          }
        case Some("yaml" | "yml") =>
          wrapFs(file.read).flatMap { str =>
            ZIO.fromEither(YamlParser.parseJson(str)).mapError { e => ConfigFileError.YamlDecodeFailure(file.pathName, e) }
          }
        case ext =>
          ZIO.fail(ConfigFileError.UnsupportedExtension(file.pathName, ext))

    override def load[A: JsonDecoder as decoder](file: Path): IO[ConfigFileError, A] =
      loadJson(file).flatMap(decodeAst[A](file.pathName, _))

    override def mergeDirectoryJson(dir: Path): IO[ConfigFileError, Json] =
      list(dir).flatMap { files =>
        ZIO.foreach(files)(loadJson).flatMap {
          case c if c.isEmpty => ZIO.fail(ConfigFileError.EmptyDirectory(dir.pathName))
          case c              => ZIO.succeed(c.reduceLeft(_ ++ _))
        }
      }

    override def mergeDirectory[A: JsonDecoder](dir: Path): IO[ConfigFileError, A] =
      mergeDirectoryJson(dir).flatMap(decodeAst[A](dir.pathName, _))

    override def loadResolvedJson(path: Path): IO[ConfigFileError, Json] =
      wrapFs(path.status).flatMap {
        case Path.Type.File           => loadJson(path)
        case Path.Type.Directory      => mergeDirectoryJson(path)
        case Path.Status.DoesNotExist => ZIO.fail(ConfigFileError.PathDoesNotExist(path.pathName))
        case Path.Type.Other          => ZIO.fail(ConfigFileError.NotFileOrDirectory(path.pathName))
      }

    override def loadResolved[A: JsonDecoder](path: Path): IO[ConfigFileError, A] =
      loadResolvedJson(path).flatMap(decodeAst[A](path.pathName, _))

    override def save[A: JsonEncoder as encoder](file: Path, value: A): IO[ConfigFileError, Unit] =
      encode[A](file, value) match
        case Left(error)     => ZIO.fail(error)
        case Right(contents) =>
          val tmp: Path = tempSiblingOf(file)
          val write: IO[FileSystemError, Unit] =
            ZIO.foreachDiscard(file.parentOption)(_.createDirectories) *>
              tmp.write(contents) *>
              tmp.moveTo(file)
          wrapFs(write).onError { _ => tmp.deleteIfExists.ignore }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Helpers
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    private def decodeAst[A](pathName: Path.PathName, json: Json)(using decoder: JsonDecoder[A]): IO[ConfigFileError, A] =
      ZIO.fromEither(decoder.decodeJsonAST(json)).mapError { e => ConfigFileError.JsonDecodeFailure(pathName, e.safeGetMessage) }

    private def encode[A](file: Path, value: A)(using encoder: JsonEncoder[A]): Either[ConfigFileError, String] =
      file.fileName.extension match
        case Some("json")         => encoder.encodeJsonStringPretty(value).asRight
        case Some("yaml" | "yml") => YamlWriter.writeJsonOf(value).asRight
        case ext                  => ConfigFileError.UnsupportedExtension(file.pathName, ext).asLeft

    private def tempSiblingOf(file: Path): Path = {
      val name: String = s".${file.fileName.name}.${PlatformCompat.randomUUID()}.tmp"
      file.parentOption.fold(file.resolve(name))(_.resolve(name))
    }

  }

}
