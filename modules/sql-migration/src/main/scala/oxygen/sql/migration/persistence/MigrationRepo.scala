package oxygen.sql.migration.persistence

import java.time.Instant
import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.MigrationError.*
import oxygen.sql.migration.persistence.conversion.dbToDomain.*
import oxygen.sql.migration.persistence.conversion.domainToDb.*
import oxygen.sql.migration.persistence.model.*
import oxygen.sql.model.TypedJsonb
import oxygen.sql.query.*
import oxygen.sql.query.dsl.*
import oxygen.sql.schema.{InputEncoder, RowRepr, TableRepr}
import zio.*

trait MigrationRepo {

  // =====| Create |=====
  def initialize: IO[ErrorInitiatingMigrations, Unit]
  def startMigration(version: Int, now: Instant): IO[ErrorPersistingMigration, Unit]
  def completeMigration(version: Int, now: Instant): IO[ErrorPersistingMigration, Unit]
  def executeStep(version: Int, step: CalculatedMigration.Step): IO[StepError, ExecutedMigration.Step]

  // =====| Get |=====
  def getMigrations: IO[ErrorFetchingMigrations, Contiguous[ExecutedMigration]]

}
object MigrationRepo {

  val layer: URLayer[Database, MigrationRepo] =
    ZLayer.fromFunction { Live.apply }

  final case class Live(
      db: Database,
  ) extends MigrationRepo {

    override def initialize: IO[ErrorInitiatingMigrations, Unit] =
      (for {
        _ <- Live.queries.createMigrationSchema.execute().unit
        _ <- Live.queries.createMigrationsTable.execute().unit
        _ <- Live.queries.createMigrationStepsTable.execute().unit
      } yield ()).mapError(ErrorInitiatingMigrations(_)).usingDb(db)

    override def startMigration(version: Int, now: Instant): IO[ErrorPersistingMigration, Unit] =
      Live.queries.insertMigration(ExecutedMigrationRow(version, now, None)).unit.mapError(ErrorPersistingMigration(version, None, _)).usingDb(db)

    override def completeMigration(version: Int, now: Instant): IO[ErrorPersistingMigration, Unit] =
      Live.queries.setCompletedAt.execute(now, version).unit.mapError(ErrorPersistingMigration(version, None, _)).usingDb(db)

    override def executeStep(version: Int, step: CalculatedMigration.Step): IO[StepError, ExecutedMigration.Step] = {
      val (effect, stepCol, sql) = evalStep(version, step)
      for {
        _ <- effect.usingDb(db)
        executedStep = ExecutedMigrationStepRow(version, step.stepNo, step.derived, TypedJsonb(stepCol), sql)
        _ <- Live.queries.insertMigrationStep(executedStep).unit.mapError(ErrorPersistingMigration(version, step.some, _)).usingDb(db)
      } yield executedStep.toDomain
    }

    override def getMigrations: IO[MigrationError.ErrorFetchingMigrations, Contiguous[ExecutedMigration]] =
      for {
        dbMigrations <- Live.queries.getMigrations.execute().contiguous.mapError(MigrationError.ErrorFetchingMigrations(_)).usingDb(db)
        dbMigrationSteps <- Live.queries.getMigrationSteps.execute().contiguous.mapError(MigrationError.ErrorFetchingMigrations(_)).usingDb(db)
        groupedSteps = dbMigrationSteps.groupMap(_.version)(_.toDomain)
      } yield dbMigrations.sortBy(_.version).map { migration =>
        ExecutedMigration(
          migration.version,
          groupedSteps.getOrElse(migration.version, Contiguous.empty).sortBy(_.stepNo),
          migration.startedAt,
          migration.completedAt,
        )
      }

    // =====| Helpers |=====

    private def evalStep(version: Int, step: CalculatedMigration.Step): (ZIO[Database, ErrorExecutingMigrationStep, Unit], MigrationStepColumn, Option[String]) =
      step.step match {
        case CalculatedMigration.StepType.Diff(diff) =>
          val query = MigrationQueries.diffToQuery(diff)
          (query.execute().unit.mapError(ErrorExecutingMigrationStep(version, step, _)), diff.toDb, query.ctx.sql.some)
      }

  }
  object Live {

    private object queries {

      val createMigrationSchema: Query =
        MigrationQueries.createSchema(SchemaRef("oxygen_migration"), true)

      val createMigrationsTable: Query =
        MigrationQueries.createTable(TableState.unsafeFromTable(TableRepr.of[ExecutedMigrationRow]), true)

      val createMigrationStepsTable: Query =
        MigrationQueries.createTable(TableState.unsafeFromTable(TableRepr.of[ExecutedMigrationStepRow]), true)

      val insertMigration: QueryI[ExecutedMigrationRow] =
        Helpers.insertInto[ExecutedMigrationRow]

      val insertMigrationStep: QueryI[ExecutedMigrationStepRow] =
        Helpers.insertInto[ExecutedMigrationStepRow]

      // TODO (KR) : update this when DSL supports updates
      val setCompletedAt: QueryI[(Instant, Int)] =
        QueryI.simple("set completed at", QueryContext.QueryType.Update)(InputEncoder.derived[(Instant, Int)]) {
          s"""UPDATE ${ExecutedMigrationRow.tableRepr.ref} m
             |  SET completed_at = ?
             |  WHERE m.version = ?
             |""".stripMargin
        }

      val getMigrations: QueryO[ExecutedMigrationRow] =
        for {
          _ <- select("get migrations")
          m <- from[ExecutedMigrationRow]
        } yield m

      val getMigrationSteps: QueryO[ExecutedMigrationStepRow] =
        for {
          _ <- select("get migration steps")
          m <- from[ExecutedMigrationStepRow]
        } yield m

    }

  }

}
