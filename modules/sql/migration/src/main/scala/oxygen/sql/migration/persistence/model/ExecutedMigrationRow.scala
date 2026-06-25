package oxygen.sql.migration.persistence.model

import java.time.Instant
import oxygen.core.Version
import oxygen.sql.migration.MigrationCodecs.given
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*

@schemaName("oxygen_migration")
@tableName("migration")
final case class ExecutedMigrationRow(
    @primaryKey version: Version,
    startedAt: Instant,
    completedAt: Option[Instant],
)
object ExecutedMigrationRow extends TableCompanion[ExecutedMigrationRow, Version](TableRepr.derived[ExecutedMigrationRow])
