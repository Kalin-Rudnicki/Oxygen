package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import oxygen.predef.core.*

final case class MigrationForeignKeyColumn(
    fkName: String,
    fkNameIsExplicit: Boolean,
    self: EntityRefColumn.TableRef,
    references: EntityRefColumn.TableRef,
    columnPairs: ArraySeq[MigrationForeignKeyColumn.Pair],
) derives JsonCodec
object MigrationForeignKeyColumn {

  final case class Pair(
      self: String,
      references: String,
  ) derives JsonCodec

}
