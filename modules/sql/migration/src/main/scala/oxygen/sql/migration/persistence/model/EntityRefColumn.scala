package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec

object EntityRefColumn {

  final case class SchemaRef(
      schema: String,
  ) derives JsonCodec

  final case class TableRef(
      schema: String,
      table: String,
  ) derives JsonCodec

  final case class ColumnRef(
      schema: String,
      table: String,
      column: String,
  ) derives JsonCodec

}
