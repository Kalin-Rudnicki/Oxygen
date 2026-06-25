package oxygen.sql.migration.model

import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.TableRepr

/**
  * A single table contributed to a [[MigrationSchema]].
  *
  * Opaque alias of `TableRepr[?]` whose only purpose is ergonomics: the given conversions let a
  * caller pass either a `TableRepr` or a `TableCompanion` (with or without a key) directly, and mix
  * them freely:
  *
  * {{{
  * MigrationSchema.of(UserRow, ConnectionRow, someTableRepr)
  * }}}
  */
opaque type AutoTableSpec <: TableRepr[?] = TableRepr[?]
object AutoTableSpec {

  given fromRepr: Conversion[TableRepr[?], AutoTableSpec] = identity

  given fromCompanion: Conversion[TableCompanion[?, ?], AutoTableSpec] = _.tableRepr

  given fromNoKeyCompanion: Conversion[TableCompanion.NoKey[?], AutoTableSpec] = _.tableRepr

}
