package oxygen.sql.migration

import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.sql.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.MigrationError.*
import oxygen.sql.migration.persistence.*
import scala.annotation.tailrec

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
      (newState, calculated) <- ZIO.fromEither { calculateMigrations(migrations.migrations) }

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

  private final case class CalculatedStep(
      derived: Boolean,
      stepType: CalculatedMigration.StepType,
  )

  private def calculateSteps(currentState: MigrationState, step: PlannedMigration.StepType): Either[StateDiffError, (MigrationState, ArraySeq[CalculatedStep])] =
    for {
      steps <- step match {
        case PlannedMigration.StepType.Diff(diff) =>
          ArraySeq(CalculatedStep(false, CalculatedMigration.StepType.Diff(diff))).asRight
        case PlannedMigration.StepType.Auto(reprs) =>
          MigrationState.fromTables(reprs).flatMap(currentState.diff).map(_.toArraySeq.map(d => CalculatedStep(true, CalculatedMigration.StepType.Diff(d))))
      }
      newState <- currentState.applyAll(steps.collect { case CalculatedStep(_, CalculatedMigration.StepType.Diff(diff)) => diff })
    } yield (newState, steps)

  private def calculateMigration(currentState: MigrationState, planned: PlannedMigration): Either[CalculationError, (MigrationState, CalculatedMigration)] =
    for {
      (actualState, growableSteps) <-
        planned.steps
          .eitherFoldLeft(
            (currentState, Growable.empty[CalculatedStep]),
          ) { case ((currentState, growableSteps), plannedStep) =>
            calculateSteps(currentState, plannedStep)
              .map { (newState, newSteps) => (newState, growableSteps ++ Growable.many(newSteps)) }
              .leftMap(ErrorDiffingState(planned.version, currentState.some, _))
          }
      stepTypes = growableSteps.toArraySeq
      _ <- Either.cond(stepTypes.nonEmpty, (), EmptyMigration(planned))
      steps = stepTypes.zipWithIndexFrom(1).map { case (CalculatedStep(derived, stepType), idx) => CalculatedMigration.Step(idx, derived, stepType) }
      expectedState <- MigrationState.fromTables(planned.tables).leftMap(ErrorDiffingState(planned.version, None, _))
      actualDiffExpected <- actualState.diff(expectedState).leftMap(ErrorDiffingState(planned.version, None, _))
      _ <- Either.cond(actualDiffExpected.isEmpty, (), MigrationDoesNotResultInExpectedState(expectedState, actualState, actualDiffExpected))
    } yield (actualState, CalculatedMigration(planned, steps))

  private def calculateMigrations(planned: ArraySeq[PlannedMigration]): Either[CalculationError, (MigrationState, ArraySeq[CalculatedMigration])] = {
    @tailrec
    def loop(
        expVersion: Int,
        currentState: MigrationState,
        queue: List[PlannedMigration],
        stack: Growable[CalculatedMigration],
    ): Either[CalculationError, (MigrationState, ArraySeq[CalculatedMigration])] =
      queue match {
        case head :: _ if head.version != expVersion =>
          UnexpectedVersion(expVersion, head).asLeft
        case head :: tail =>
          calculateMigration(currentState, head) match {
            case Right((newState, calculated)) => loop(expVersion + 1, newState, tail, stack :+ calculated)
            case Left(error)                   => error.asLeft
          }
        case Nil =>
          (currentState, stack.toArraySeq).asRight
      }

    loop(1, MigrationState.empty, planned.sortBy(_.version).toList, Growable.empty)
  }

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
