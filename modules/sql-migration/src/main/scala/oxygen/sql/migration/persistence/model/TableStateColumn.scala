package oxygen.sql.migration.persistence.model

import oxygen.core.collection.Contiguous
import oxygen.json.JsonCodec

final case class TableStateColumn(
    tableName: EntityRefColumn.TableRef,
    primaryKeyColumns: Set[String],
    columns: Contiguous[ColumnColumn],
) derives JsonCodec
