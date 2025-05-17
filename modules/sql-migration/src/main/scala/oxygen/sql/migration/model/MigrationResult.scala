package oxygen.sql.migration.model

import oxygen.core.collection.Contiguous

final case class MigrationResult(
    state: MigrationState,
    executed: Contiguous[ExecutedMigration],
)
