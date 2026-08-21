package oxygen.sql.migration.compat

import oxygen.schema.compat.{AddedRemovedBoth, FromToValues}
import oxygen.sql.migration.compat.DbComparisonResult.*
import oxygen.sql.migration.model.*
import oxygen.sql.schema.Column

/**
  * Pure, DB-free compatibility spec for the migration schema. `compare(from, to)` produces a rich,
  * per-entity [[DbComparisonResult]] whose `.compatibility` collapses to the binary
  * `MigrationCompatibility` the migration harness consumes. See [[DbComparisonResult]] for the rule
  * rationale (it agrees with `MigrationGenerator.classifyOne` where they overlap, and adds
  * per-column type widening/narrowing on top).
  */
object DbCompatibilitySpec {

  def compare(from: MigrationState, to: MigrationState): DbComparisonResult =
    DbComparisonResult(
      extensions = AddedRemovedBoth.Many.simpleSortedSet(from.extensions, to.extensions),
      schemas = AddedRemovedBoth.Many.simpleSortedSet(from.schemas, to.schemas),
      tables = diffMap(from.tables, to.tables)(compareTable),
    )

  private def compareTable(from: TableState, to: TableState): TableComparison =
    TableComparison(
      tableRef = to.tableName,
      columns = diffSeq(from.columns, to.columns)(_.name)(compareColumn),
      primaryKey = FromToValues(from.pkColNames, to.pkColNames),
      foreignKeys = diffSeq(from.foreignKeys, to.foreignKeys)(_.fkName)((f, t) => ConstraintComparison(t.ref, FromToValues(f, t))),
      indices = diffSeq(from.indices, to.indices)(_.idxName)((f, t) => ConstraintComparison(t.ref, FromToValues(f, t))),
    )

  private def compareColumn(from: Column, to: Column): ColumnComparison =
    ColumnComparison(
      name = to.name,
      nullable = FromToValues(from.nullable, to.nullable),
      columnType = TypeComparison.compare(from.columnType, to.columnType),
    )

  private def diffSeq[K: Ordering, V, B](from: Seq[V], to: Seq[V])(key: V => K)(both: (V, V) => B): AddedRemovedBoth.Many[V, B] =
    diffMap(from.map(v => key(v) -> v).toMap, to.map(v => key(v) -> v).toMap)(both)

  private def diffMap[K: Ordering, V, B](from: Map[K, V], to: Map[K, V])(both: (V, V) => B): AddedRemovedBoth.Many[V, B] = {
    val addedKeys = (to.keySet -- from.keySet).toSeq.sorted
    val removedKeys = (from.keySet -- to.keySet).toSeq.sorted
    val bothKeys = (from.keySet & to.keySet).toSeq.sorted
    AddedRemovedBoth.Many(
      added = addedKeys.map(to(_)),
      removed = removedKeys.map(from(_)),
      both = bothKeys.map(k => both(from(k), to(k))),
    )
  }

}
