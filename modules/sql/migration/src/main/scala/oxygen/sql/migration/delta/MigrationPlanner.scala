package oxygen.sql.migration.delta

import oxygen.predef.core.*
import oxygen.sql.migration.model.*

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
      ordered <- orderDeterministically(all.toArraySeq)
    } yield ordered

  /**
    * Total, reproducible ordering of the derived diffs: by [[StateDiff.applicationOrder]] then by
    * target entity. Two diffs sharing both (same operation priority on the same entity) are a genuine
    * duplicate and are rejected rather than ordered arbitrarily.
    */
  private def orderDeterministically(diffs: ArraySeq[StateDiff.Derivable]): Either[StateDiffError, ArraySeq[StateDiff.Derivable]] =
    diffs.groupBy(d => (d.applicationOrder, d.entityRef)).toSeq.traverse { (_, grouped) =>
      Either.cond(grouped.sizeIs == 1, grouped.head, StateDiffError.DiffError(grouped.head.entityRef, StateDiffError.Cause.DuplicateDiff))
    }.map { distinct => ArraySeq.from(distinct).sortBy(d => (d.applicationOrder, d.entityRef.toString)) }

}
