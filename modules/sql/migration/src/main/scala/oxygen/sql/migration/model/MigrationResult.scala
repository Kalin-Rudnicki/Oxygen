package oxygen.sql.migration.model

import scala.collection.immutable.ArraySeq

final case class MigrationResult(
    state: MigrationState,
    executed: ArraySeq[ExecutedMigration],
)
