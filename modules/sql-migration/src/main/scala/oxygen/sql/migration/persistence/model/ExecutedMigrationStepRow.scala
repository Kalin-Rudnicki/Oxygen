package oxygen.sql.migration.persistence.model

import oxygen.sql.model.TypedJsonb
import oxygen.sql.schema.*

@schemaName("oxygen_migration")
@tableName("migration_step")
final case class ExecutedMigrationStepRow(
    @primaryKey version: Int,
    @primaryKey stepNo: Int,
    derived: Boolean,
    step: TypedJsonb[MigrationStepColumn],
    sql: Option[String],
)
object ExecutedMigrationStepRow {
  given tableRepr: TableRepr[ExecutedMigrationStepRow, (Int, Int)] = TableRepr.derived
}
