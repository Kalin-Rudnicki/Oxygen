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
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.schema.TableRepr
import zio.*

trait MigrationRepo {

  // =====| Create |=====
  def initialize: IO[ErrorInitiatingMigrations, Unit]
  def startMigration(version: Int, now: Instant): IO[ErrorPersistingMigration, Unit]
  def completeMigration(version: Int, now: Instant): IO[ErrorPersistingMigration, Unit]
  def executeStep(version: Int, step: CalculatedMigration.Step): IO[StepError, ExecutedMigration.Step]

  // =====| Get |=====
  def getMigrations: IO[ErrorFetchingMigrations, ArraySeq[ExecutedMigration]]

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

    override def getMigrations: IO[MigrationError.ErrorFetchingMigrations, ArraySeq[ExecutedMigration]] =
      for {
        dbMigrations <- Live.queries.getMigrations.execute().arraySeq.mapError(MigrationError.ErrorFetchingMigrations(_)).usingDb(db)
        dbMigrationSteps <- Live.queries.getMigrationSteps.execute().arraySeq.mapError(MigrationError.ErrorFetchingMigrations(_)).usingDb(db)
        groupedSteps = dbMigrationSteps.groupMap(_.version)(_.toDomain)
      } yield dbMigrations.sortBy(_.version).map { migration =>
        ExecutedMigration(
          migration.version,
          groupedSteps.getOrElse(migration.version, ArraySeq.empty[ExecutedMigration.Step]).sortBy(_.stepNo),
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
        ExecutedMigrationRow.insert

      val insertMigrationStep: QueryI[ExecutedMigrationStepRow] =
        ExecutedMigrationStepRow.insert

      val setCompletedAt: QueryI[(Instant, Int)] =
        QueryI.compile("setCompleteAt") {
          for {
            at <- input[Instant]
            v <- input[Int]
            (m, set) <- update[ExecutedMigrationRow]
            _ <- where if m.version == v
            _ <- set(_.completedAt.get := at)
          } yield ()
        }

      val getMigrations: QueryO[ExecutedMigrationRow] =
        ExecutedMigrationRow.selectAll

      val getMigrationSteps: QueryO[ExecutedMigrationStepRow] =
        ExecutedMigrationStepRow.selectAll

    }

  }

}
