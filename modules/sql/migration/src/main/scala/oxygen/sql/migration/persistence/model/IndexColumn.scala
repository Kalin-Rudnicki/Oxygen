package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import oxygen.predef.core.*

final case class IndexColumn(
    idxName: String,
    idxNameIsExplicit: Boolean,
    self: EntityRefColumn.TableRef,
    unique: Boolean,
    columns: ArraySeq[String],
) derives JsonCodec
