package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import scala.collection.immutable.ArraySeq

final case class TableStateColumn(
    tableName: EntityRefColumn.TableRef,
    primaryKeyColumns: Set[String],
    columns: ArraySeq[ColumnColumn],
) derives JsonCodec
