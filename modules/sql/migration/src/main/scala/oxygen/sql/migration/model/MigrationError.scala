package oxygen.sql.migration.model

import oxygen.core.Version
import oxygen.predef.core.*
import oxygen.sql.error.QueryError
import oxygen.sql.migration.persistence.MigrationFs.MigrationFsError
import oxygen.sql.migration.persistence.model.PersistedMigrationFile

sealed trait MigrationError extends Throwable {

  override final def getMessage: String =
    toIndentedString.toString

  final def toIndentedString: IndentedString =
    this match {
      case MigrationError.ErrorInitiatingMigrations(cause) =>
        IndentedString.section("error initiating migrations")(
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.ErrorFetchingMigrations(cause) =>
        IndentedString.section("error fetching migrations")(
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.ErrorReadingMigrationFiles(cause) =>
        IndentedString.section("error reading migration files")(
          s"cause: $cause",
        )
      case MigrationError.InvalidMigrationVersion(value) =>
        IndentedString.section("invalid migration version")(
          s"value: $value",
        )
      case MigrationError.ErrorPersistingMigration(version, step, cause) =>
        IndentedString.section("error persisting migration")(
          s"version: $version",
          step.map(step => s"step: $step"),
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.ErrorExecutingMigrationStep(version, step, cause: QueryError) =>
        IndentedString.section("error executing migration step")(
          s"version: $version",
          s"step: $step",
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.ErrorExecutingMigrationStep(version, step, cause) =>
        IndentedString.section("error executing migration step")(
          s"version: $version",
          s"step: $step",
          IndentedString.section("cause:")(
            IndentedString.keyValue("type: ", cause.getClass.getName),
            IndentedString.keyValue("message: ", cause.safeGetMessage),
          ),
        )
      case MigrationError.MissingMigration(executed) =>
        IndentedString.section("missing migration (executed in DB but absent from filesystem)")(
          IndentedString.section("executed:")(executed.toIndentedString),
        )
      case MigrationError.MigrationsDiffer(executed, file) =>
        IndentedString.section("migrations differ (executed in DB disagrees with filesystem)")(
          IndentedString.section("executed:")(executed.toIndentedString),
          s"filesystem version: ${file.version}",
        )
      case MigrationError.OutOfOrderMigration(version, lastExecuted) =>
        IndentedString.section("out-of-order migration (not supported)")(
          s"version: $version",
          s"last executed: $lastExecuted",
        )
      case MigrationError.ErrorVerifyingMigrations(cause) =>
        IndentedString.section("error verifying migrations against current code")(
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.MigrationsStale(pending) =>
        IndentedString.section("committed migrations are stale relative to current code (regenerate them)")(
          IndentedString.section("missing diffs:")(pending.map(_.toIndentedString)),
        )
    }

}
object MigrationError {

  type StepError = ErrorPersistingMigration | ErrorExecutingMigrationStep

  final case class ErrorInitiatingMigrations(
      cause: QueryError,
  ) extends MigrationError

  final case class ErrorFetchingMigrations(
      cause: QueryError,
  ) extends MigrationError

  final case class ErrorReadingMigrationFiles(
      cause: MigrationFsError,
  ) extends MigrationError

  final case class InvalidMigrationVersion(
      value: String,
  ) extends MigrationError

  final case class ErrorPersistingMigration(
      version: Version,
      step: Option[PersistedMigrationFile.Step],
      cause: QueryError,
  ) extends MigrationError

  final case class ErrorExecutingMigrationStep(
      version: Version,
      step: PersistedMigrationFile.Step,
      cause: Throwable,
  ) extends MigrationError

  final case class MissingMigration(
      executed: ExecutedMigration,
  ) extends MigrationError

  final case class MigrationsDiffer(
      executed: ExecutedMigration,
      file: PersistedMigrationFile,
  ) extends MigrationError

  final case class OutOfOrderMigration(
      version: Version,
      lastExecuted: Version,
  ) extends MigrationError

  final case class ErrorVerifyingMigrations(
      cause: StateDiffError,
  ) extends MigrationError

  /** The committed filesystem migrations do not reproduce the current-code schema. */
  final case class MigrationsStale(
      pending: ArraySeq[StateDiff],
  ) extends MigrationError

}
