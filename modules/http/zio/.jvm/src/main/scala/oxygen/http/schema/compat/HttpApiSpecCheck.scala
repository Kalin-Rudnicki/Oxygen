package oxygen.http.schema.compat

import oxygen.http.schema.compiled.RawCompiledApiSpec
import oxygen.json.JsonCodec
import oxygen.predef.json.*
import oxygen.zio.error.FileSystemError
import oxygen.zio.system.Path
import scala.collection.immutable.ArraySeq
import zio.*

/**
  * The "avro paradigm" harness for the compiled HTTP API spec -- the endpoint-schema analogue of
  * `oxygen.sql.migration.MigrationCheck`. Diffs the current-code spec against the committed JSON file
  * and either confirms it is up to date or (when permitted) writes the new spec.
  *
  *   - In CI (`allowUpdate = false`), a stale/absent committed spec yields [[Outcome.PendingUpdate]] --
  *     the caller fails the build, signalling the committed doc is out of date.
  *   - Locally (`allowUpdate = true`), the new spec is written. A change that is *incompatible* for
  *     clients additionally requires `allowIncompatible = true`, else [[Outcome.BlockedIncompatible]].
  */
object HttpApiSpecCheck {

  final case class Config(allowUpdate: Boolean, allowIncompatible: Boolean)
  object Config {

    val allowUpdateEnv: String = "OXYGEN_HTTP_ALLOW_UPDATE"
    val allowIncompatibleEnv: String = "OXYGEN_HTTP_ALLOW_INCOMPATIBLE"

    /** CI default: never write, never allow incompatible. */
    val ci: Config = Config(allowUpdate = false, allowIncompatible = false)

    private def truthy(value: Option[String]): Boolean =
      value.map(_.trim.toLowerCase).exists(v => v == "true" || v == "1" || v == "yes")

    val fromEnv: ZIO[Any, SecurityException, Config] =
      for {
        update <- System.env(allowUpdateEnv)
        incompatible <- System.env(allowIncompatibleEnv)
      } yield Config(truthy(update), truthy(incompatible))

  }

  enum Outcome {
    case UpToDate
    case Wrote(path: Path)
    case PendingUpdate(comparison: HttpApiSpecComparison.Result)
    case BlockedIncompatible(comparison: HttpApiSpecComparison.Result)
  }

  enum HttpApiSpecCheckError {
    case Fs(cause: FileSystemError)
    case Decode(fileName: String, message: String)
  }

  private val codec: JsonCodec[RawCompiledApiSpec] = JsonCodec[RawCompiledApiSpec]

  private def canonical(spec: RawCompiledApiSpec): String = codec.encoder.encodeJsonStringCompact(spec.withoutLineNos)

  def check(
      path: Path,
      currentSpec: RawCompiledApiSpec,
      config: Config,
  ): IO[HttpApiSpecCheckError, Outcome] = {
    val current: RawCompiledApiSpec = currentSpec.withoutLineNos
    path.exists.mapError(HttpApiSpecCheckError.Fs(_)).flatMap {
      case false =>
        // Genesis: nothing committed yet -- a new file is always compatible.
        onDifference(path, current, HttpApiSpecComparison.Result(ArraySeq.empty), config)
      case true =>
        for {
          contents <- path.read.mapError(HttpApiSpecCheckError.Fs(_))
          committed <- ZIO.fromEither(contents.fromJsonString[RawCompiledApiSpec]).mapError(e => HttpApiSpecCheckError.Decode(path.fileName.name, e.getMessage))
          outcome <-
            if canonical(committed) == canonical(current) then ZIO.succeed(Outcome.UpToDate)
            else onDifference(path, current, HttpApiSpecComparison.compare(committed.withoutLineNos, current), config)
        } yield outcome
    }
  }

  private def onDifference(
      path: Path,
      current: RawCompiledApiSpec,
      comparison: HttpApiSpecComparison.Result,
      config: Config,
  ): IO[HttpApiSpecCheckError, Outcome] =
    if !config.allowUpdate then ZIO.succeed(Outcome.PendingUpdate(comparison))
    else if comparison.compatibility == HttpApiSpecComparison.Compatibility.Incompatible && !config.allowIncompatible then ZIO.succeed(Outcome.BlockedIncompatible(comparison))
    else write(path, current).mapBoth(HttpApiSpecCheckError.Fs(_), _ => Outcome.Wrote(path))

  private def write(path: Path, spec: RawCompiledApiSpec): IO[FileSystemError, Path] =
    for {
      _ <- path.parentOption.fold[IO[FileSystemError, Unit]](ZIO.unit)(_.createDirectories)
      _ <- path.write(spec.toJsonStringPretty)
    } yield path

}
