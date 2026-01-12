package oxygen.sql.migration

import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.sql.*
import oxygen.sql.migration.delta.MigrationPlanner
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.MigrationError.*
import oxygen.sql.migration.persistence.*
import zio.*

final class MigrationService(
    atomically: Atomically,
    repo: MigrationRepo,
    config: MigrationConfig,
) {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def migrate(migrations: => Migrations): IO[MigrationError, MigrationResult] =
    for {
      _ <- ZIO.logDebug("Making sure oxygen migration is initialized")
      _ <- repo.initialize

      _ <- ZIO.logDebug("Evaluating lazy migrations")
      migrations <- ZIO.succeed { migrations }

      _ <- ZIO.logDebug("Calculating planned migrations")
      (newState, calculated) <- ZIO.fromEither { MigrationPlanner.calculateMigrations(migrations.migrations) }

      _ <- ZIO.logDebug("Loading executed migrations")
      executed <- repo.getMigrations.map(_.sortBy(_.version))

      toExecute <- ZIO.fromEither {
        calculated
          .zipUsing(executed)(
            _.some.asRight,
            MissingPlannedMigration(_).asLeft,
            (calc, exe) => Either.cond(exe.comesFrom(calc), None, MigrationsDiffer(exe, calc)),
          )
          .sequence
          .map(_.flatten)
      }

      _ <- ZIO.logDebug("Executing migrations")
      _ <- ZIO.logInfo("No new migrations to run").whenDiscard(toExecute.isEmpty)
      executed <- config.atomicity match {
        case MigrationConfig.Atomicity.None         => ZIO.foreach(toExecute)(execute)
        case MigrationConfig.Atomicity.PerMigration => ZIO.foreach(toExecute)(execute(_) @@ atomically)
        case MigrationConfig.Atomicity.AllOrNothing => ZIO.foreach(toExecute)(execute) @@ atomically
      }

    } yield MigrationResult(newState, executed)

  // TODO (KR) : support rollback

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def execute(calculated: CalculatedMigration): IO[StepError, ExecutedMigration] =
    ZIO.withLogSpan(s"Migration #${calculated.version}") {
      for {
        startedAt <- Clock.instant
        _ <- ZIO.logInfo("Starting migration")
        _ <- repo.startMigration(calculated.version, startedAt)

        steps <- ZIO.foreach(calculated.steps)(repo.executeStep(calculated.version, _))

        completedAt <- Clock.instant
        _ <- ZIO.logInfo("Migration complete")
        _ <- repo.completeMigration(calculated.version, completedAt)
      } yield ExecutedMigration(calculated.version, steps, startedAt, completedAt.some)
    }

}
object MigrationService {

  val layer: URLayer[Atomically & MigrationRepo & MigrationConfig, MigrationService] =
    ZLayer.fromFunction { MigrationService.apply }

  def migrate(migrations: => Migrations): ZIO[MigrationService, MigrationError, MigrationResult] =
    ZIO.serviceWithZIO[MigrationService](_.migrate(migrations))

  def migrateLayer(migrations: => Migrations): ZLayer[MigrationService, MigrationError, Unit] =
    ZLayer { migrate(migrations) }.unit

}
