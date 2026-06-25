package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import scala.collection.immutable.ArraySeq

/**
  * On-disk representation of a single migration version.
  *
  * Each file is self-contained: it carries the full resulting [[state]] snapshot as well as the
  * [[diff]] that transformed the previous version's state into it. Storing the snapshot is what
  * unlocks reverse / cross-version diffing later (computed by diffing two snapshots), rather than
  * needing each drop-style diff to carry enough information to invert itself.
  *
  * `version`/`previousVersion` are stored as strings; the filename is the authoritative source of
  * the version, and the string form is unambiguous (parsed to `oxygen.core.Version` on load).
  *
  * `sql` is intentionally NOT stored here -- it is derivable from the diff via
  * `MigrationQueries.diffToQuery`, and is only recorded in the DB execution log at apply time.
  */
final case class PersistedMigrationFile(
    formatVersion: Int,
    version: String,
    previousVersion: Option[String],
    compatibility: MigrationCompatibility,
    diff: ArraySeq[PersistedMigrationFile.Step],
    state: MigrationStateColumn,
) derives JsonCodec
object PersistedMigrationFile {

  /** Envelope schema version. Bump when the on-disk shape changes incompatibly. */
  val currentFormatVersion: Int = 1

  final case class Step(
      stepNo: Int,
      derived: Boolean,
      step: MigrationStepColumn,
  ) derives JsonCodec

}
