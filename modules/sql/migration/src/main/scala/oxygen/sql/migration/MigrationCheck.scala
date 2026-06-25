package oxygen.sql.migration

import oxygen.sql.migration.MigrationGenerator.GenerateResult
import oxygen.sql.migration.model.{MigrationSchema, MigrationState, StateDiffError}
import oxygen.sql.migration.persistence.MigrationFs
import oxygen.sql.migration.persistence.MigrationFs.MigrationFsError
import oxygen.sql.migration.persistence.conversion.dbToDomain.*
import oxygen.sql.migration.persistence.model.{MigrationCompatibility, PersistedMigrationFile}
import oxygen.zio.system.Path
import zio.*

/**
  * The "avro paradigm" harness: diff the current-code tables against the persisted filesystem
  * history and either confirm it is up to date or (when permitted) write the new migration file.
  *
  * Intended to be driven from a test or a CLI:
  *   - In CI (`allowUpdate = false`), a pending migration yields [[Outcome.PendingUpdate]] -- the
  *     caller fails the build, signalling the committed FS history is stale.
  *   - Locally (`allowUpdate = true`), the new file is written. An incompatible migration
  *     additionally requires `allowIncompatible = true`, else [[Outcome.BlockedIncompatible]].
  */
object MigrationCheck {

  final case class Config(allowUpdate: Boolean, allowIncompatible: Boolean)
  object Config {

    val allowUpdateEnv: String = "OXYGEN_MIGRATION_ALLOW_UPDATE"
    val allowIncompatibleEnv: String = "OXYGEN_MIGRATION_ALLOW_INCOMPATIBLE"

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
    case Wrote(file: PersistedMigrationFile, path: Path)
    case PendingUpdate(file: PersistedMigrationFile, compatibility: MigrationCompatibility)
    case BlockedIncompatible(file: PersistedMigrationFile)
  }

  enum MigrationCheckError {
    case Fs(cause: MigrationFsError)
    case Diff(cause: StateDiffError)
  }

  def check(
      dir: Path,
      schema: MigrationSchema,
      config: Config,
  ): IO[MigrationCheckError, Outcome] = {
    val fs = MigrationFs(dir)
    for {
      target <- ZIO.fromEither(MigrationState.fromTables(schema.tables)).mapError(MigrationCheckError.Diff(_))
      latest <- fs.latestSnapshot.mapError(MigrationCheckError.Fs(_))
      previous = latest.map { case (version, file) => (version, file.state.toDomain) }
      result <- ZIO.fromEither(MigrationGenerator.generate(previous, target)).mapError(MigrationCheckError.Diff(_))
      outcome <- result match
        case GenerateResult.UpToDate                       => ZIO.succeed(Outcome.UpToDate)
        case GenerateResult.Generated(file, compatibility) =>
          if !config.allowUpdate then ZIO.succeed(Outcome.PendingUpdate(file, compatibility))
          else if compatibility == MigrationCompatibility.Incompatible && !config.allowIncompatible then ZIO.succeed(Outcome.BlockedIncompatible(file))
          else fs.write(file).mapBoth(MigrationCheckError.Fs(_), Outcome.Wrote(file, _))
    } yield outcome
  }

}
