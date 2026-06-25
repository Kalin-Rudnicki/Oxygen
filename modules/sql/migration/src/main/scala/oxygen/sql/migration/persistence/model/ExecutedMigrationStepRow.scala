package oxygen.sql.migration.persistence.model

import oxygen.core.Version
import oxygen.sql.migration.MigrationCodecs.given
import oxygen.sql.model.TypedJsonb
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*

@schemaName("oxygen_migration")
@tableName("migration_step")
final case class ExecutedMigrationStepRow(
    @primaryKey version: Version,
    @primaryKey stepNo: Int,
    derived: Boolean,
    step: TypedJsonb[MigrationStepColumn],
    sql: Option[String],
)
object ExecutedMigrationStepRow extends TableCompanion[ExecutedMigrationStepRow, (Version, Int)](TableRepr.derived[ExecutedMigrationStepRow])
