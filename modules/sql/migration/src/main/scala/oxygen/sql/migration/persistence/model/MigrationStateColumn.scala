package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import scala.collection.immutable.ArraySeq

/**
  * Serializable snapshot of a whole [[oxygen.sql.migration.model.MigrationState]].
  *
  * There is no DB-side serializer for a full migration state (the DB only stores per-step),
  * so this exists primarily for the filesystem migration files ([[PersistedMigrationFile]]).
  */
final case class MigrationStateColumn(
    extensions: Set[String],
    schemas: Set[String],
    tables: ArraySeq[TableStateColumn],
) derives JsonCodec
