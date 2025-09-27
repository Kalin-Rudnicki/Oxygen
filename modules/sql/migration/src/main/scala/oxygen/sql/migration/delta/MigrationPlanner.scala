package oxygen.sql.migration.delta

import oxygen.predef.core.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.MigrationError.*
import scala.annotation.tailrec

object MigrationPlanner {

  def diffStates(
      currentState: MigrationState,
      targetState: MigrationState,
  ): Either[StateDiffError, ArraySeq[StateDiff.Derivable]] =
    for {
      p1 <- StateDiffer.phase1.diffs(currentState, targetState)
      p2 <- StateDiffer.phase2.diffs(currentState, targetState)
      all = (p1 ++ p2).flatMap {
        case StateDiff.AlterTable.CreateTable(table) =>
          // this is done to avoid having to worry about what order tables are created in, and so forth
          Growable.single(StateDiff.AlterTable.CreateTable(table.copy(foreignKeys = ArraySeq.empty, indices = ArraySeq.empty))) ++
            Growable.many(table.foreignKeys).map(StateDiff.AlterForeignKey.CreateForeignKey(_)) ++
            Growable.many(table.indices).map(StateDiff.AlterIndex.CreateIndex(_))
        case s => Growable.single(s)
      }
    } yield all.toArraySeq.sortBy(_.applicationOrder)

  def calculateSteps(currentState: MigrationState, step: PlannedMigration.StepType): Either[StateDiffError, ArraySeq[CalculatedMigration.CalculatedStep]] =
    step match {
      case PlannedMigration.StepType.Diff(diff: StateDiff.CanOnlyBeSpecified) =>
        for {
          extras <- StateDiffer.phase3.diffs(currentState, diff)
          calculatedExtras = extras.toArraySeq.sortBy(_.applicationOrder).map { d => CalculatedMigration.CalculatedStep(true, CalculatedMigration.StepType.Diff(d)) }
          calculatedSpecified = CalculatedMigration.CalculatedStep(false, CalculatedMigration.StepType.Diff(diff))
        } yield calculatedExtras :+ calculatedSpecified
      case PlannedMigration.StepType.Diff(diff) =>
        ArraySeq(CalculatedMigration.CalculatedStep(false, CalculatedMigration.StepType.Diff(diff))).asRight
      case PlannedMigration.StepType.Auto(tables) =>
        for {
          targetState <- MigrationState.fromTables(tables)
          all <- diffStates(currentState, targetState)
        } yield all.map { d => CalculatedMigration.CalculatedStep(true, CalculatedMigration.StepType.Diff(d)) }
    }

  def calculateAndApplySteps(currentState: MigrationState, step: PlannedMigration.StepType): Either[StateDiffError, (MigrationState, ArraySeq[CalculatedMigration.CalculatedStep])] =
    for {
      steps <- calculateSteps(currentState, step)
      newState <- DiffApplier.applyAll(currentState, steps.collect { case CalculatedMigration.CalculatedStep(_, CalculatedMigration.StepType.Diff(diff)) => diff })
    } yield (newState, steps)

  def calculateSteps(currentState: MigrationState, planned: PlannedMigration): Either[CalculationError, (MigrationState, Growable[CalculatedMigration.CalculatedStep])] = {
    @tailrec
    def loop(
        currentState: MigrationState,
        queue: List[PlannedMigration.StepType],
        acc: Growable[CalculatedMigration.CalculatedStep],
    ): Either[CalculationError, (MigrationState, Growable[CalculatedMigration.CalculatedStep])] =
      queue match {
        case head :: tail =>
          calculateAndApplySteps(currentState, head) match {
            case Right((newState, newSteps)) => loop(newState, tail, acc ++ Growable.many(newSteps))
            case Left(error)                 => ErrorDiffingState(planned.version, currentState.some, error).asLeft
          }
        case Nil =>
          (currentState, acc).asRight
      }

    loop(currentState, planned.steps.toList, Growable.empty)
  }

  def calculateMigration(currentState: MigrationState, planned: PlannedMigration): Either[CalculationError, (MigrationState, CalculatedMigration)] =
    for {
      (stateResultingFromSteps, growableSteps) <- calculateSteps(currentState, planned)
      stepTypes = growableSteps.toArraySeq
      _ <- Either.cond(stepTypes.nonEmpty, (), EmptyMigration(planned))
      steps = stepTypes.zipWithIndexFrom(1).map { case (CalculatedMigration.CalculatedStep(derived, stepType), idx) => CalculatedMigration.Step(idx, derived, stepType) }
      expectedState <- MigrationState.fromTables(planned.tables).leftMap(ErrorDiffingState(planned.version, None, _))
      actualDiffExpected <- diffStates(stateResultingFromSteps, expectedState).leftMap(ErrorDiffingState(planned.version, None, _))
      _ <- Either.cond(actualDiffExpected.isEmpty, (), MigrationDoesNotResultInExpectedState(expectedState, stateResultingFromSteps, actualDiffExpected))
    } yield (stateResultingFromSteps, CalculatedMigration(planned, steps))

  def calculateMigrations(planned: ArraySeq[PlannedMigration]): Either[CalculationError, (MigrationState, ArraySeq[CalculatedMigration])] = {
    @tailrec
    def loop(
        expVersion: Int,
        currentState: MigrationState,
        queue: List[PlannedMigration],
        acc: Growable[CalculatedMigration],
    ): Either[CalculationError, (MigrationState, ArraySeq[CalculatedMigration])] =
      queue match {
        case head :: _ if head.version != expVersion =>
          UnexpectedVersion(expVersion, head).asLeft
        case head :: tail =>
          calculateMigration(currentState, head) match {
            case Right((newState, calculated)) => loop(expVersion + 1, newState, tail, acc :+ calculated)
            case Left(error)                   => error.asLeft
          }
        case Nil =>
          (currentState, acc.toArraySeq).asRight
      }

    loop(1, MigrationState.empty, planned.sortBy(_.version).toList, Growable.empty)
  }

}
