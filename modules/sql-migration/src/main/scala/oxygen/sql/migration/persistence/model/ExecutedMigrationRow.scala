package oxygen.sql.migration.persistence.model

import java.time.Instant
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*

@schemaName("oxygen_migration")
@tableName("migration")
final case class ExecutedMigrationRow(
    @primaryKey version: Int,
    startedAt: Instant,
    completedAt: Option[Instant],
)
object ExecutedMigrationRow extends TableCompanion[ExecutedMigrationRow, Int](TableRepr.derived[ExecutedMigrationRow])
