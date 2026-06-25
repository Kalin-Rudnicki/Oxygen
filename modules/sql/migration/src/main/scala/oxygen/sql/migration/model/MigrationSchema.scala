package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.schema.TableRepr

/**
  * The current-code schema: the set of tables the migration system should manage.
  *
  * This is the filesystem-first replacement for the old `PlannedMigration` -- migration *history*
  * lives on disk, so code only ever declares the *latest* desired set of tables. The generator /
  * verifier diff this against the persisted filesystem state.
  */
final case class MigrationSchema(
    tables: ArraySeq[TableRepr[?]],
)
object MigrationSchema {

  val empty: MigrationSchema = MigrationSchema(ArraySeq.empty)

  /** Accepts `TableRepr`s and/or `TableCompanion`s (via [[AutoTableSpec]] conversions), mixed freely. */
  def of(tables: AutoTableSpec*): MigrationSchema = MigrationSchema(tables.toArraySeq)

}
