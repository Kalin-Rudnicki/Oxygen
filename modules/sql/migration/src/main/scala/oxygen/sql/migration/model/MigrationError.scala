package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.error.QueryError

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
      case MigrationError.ErrorPersistingMigration(version, step, cause) =>
        IndentedString.section("error persisting migration")(
          s"version: $version",
          step.map(step => IndentedString.section("step:")(step.toIndentedString)),
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.ErrorExecutingMigrationStep(version, step, cause: QueryError) =>
        IndentedString.section("error executing migration step")(
          s"version: $version",
          IndentedString.section("step:")(step.toIndentedString),
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.ErrorExecutingMigrationStep(version, step, cause) =>
        IndentedString.section("error executing migration step")(
          s"version: $version",
          IndentedString.section("step:")(step.toIndentedString),
          IndentedString.section("cause:")(
            IndentedString.keyValue("type: ", cause.getClass.getName),
            IndentedString.keyValue("message: ", cause.safeGetMessage),
          ),
        )
      case MigrationError.ErrorDiffingState(version, before, cause) =>
        IndentedString.section("error diffing state")(
          s"version: $version",
          before.map { before => IndentedString.section("before:")(before.toIndentedString) },
          IndentedString.section("cause:")(cause.toIndentedString),
        )
      case MigrationError.EmptyMigration(planned) =>
        IndentedString.section("empty migration")(
          IndentedString.section("planned:")(planned.toIndentedString),
        )
      case MigrationError.MissingPlannedMigration(executed) =>
        IndentedString.section("missing planned migration")(
          IndentedString.section("executed:")(executed.toIndentedString),
        )
      case MigrationError.UnexpectedVersion(expected, planned) =>
        IndentedString.section("unexpected version")(
          s"expected: $expected",
          IndentedString.section("planned:")(planned.toIndentedString),
        )
      case MigrationError.MigrationsDiffer(executed, calculated) =>
        IndentedString.section("migrations differ")(
          IndentedString.section("executed:")(executed.toIndentedString),
          IndentedString.section("calculated:")(calculated.toIndentedString),
        )
      case MigrationError.MigrationDoesNotResultInExpectedState(expected, actual, steps) =>
        IndentedString.section("migration does not result in expected state")(
          IndentedString.section("expected:")(expected.toIndentedString),
          IndentedString.section("actual:")(actual.toIndentedString),
          IndentedString.section("steps from actual to expected:")(steps.map(_.toIndentedString)),
        )
    }

}
object MigrationError {

  type StepError = ErrorPersistingMigration | ErrorExecutingMigrationStep
  type CalculationError = ErrorDiffingState | EmptyMigration | UnexpectedVersion | MigrationDoesNotResultInExpectedState

  final case class ErrorInitiatingMigrations(
      cause: QueryError,
  ) extends MigrationError

  final case class ErrorFetchingMigrations(
      cause: QueryError,
  ) extends MigrationError

  final case class ErrorPersistingMigration(
      version: Int,
      step: Option[CalculatedMigration.Step],
      cause: QueryError,
  ) extends MigrationError

  final case class ErrorExecutingMigrationStep(
      version: Int,
      step: CalculatedMigration.Step,
      cause: Throwable,
  ) extends MigrationError

  final case class ErrorDiffingState(
      version: Int,
      before: Option[MigrationState],
      cause: StateDiffError,
  ) extends MigrationError

  final case class EmptyMigration(
      planned: PlannedMigration,
  ) extends MigrationError

  final case class MissingPlannedMigration(
      executed: ExecutedMigration,
  ) extends MigrationError

  final case class UnexpectedVersion(
      expected: Int,
      planned: PlannedMigration,
  ) extends MigrationError

  final case class MigrationsDiffer(
      executed: ExecutedMigration,
      calculated: CalculatedMigration,
  ) extends MigrationError

  final case class MigrationDoesNotResultInExpectedState(
      expected: MigrationState,
      actual: MigrationState,
      steps: ArraySeq[StateDiff],
  ) extends MigrationError

}
