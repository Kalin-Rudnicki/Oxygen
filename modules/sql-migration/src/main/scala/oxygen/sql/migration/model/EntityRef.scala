package oxygen.sql.migration.model

sealed trait EntityRef
object EntityRef {

  final case class SchemaRef(schemaName: String) extends EntityRef {
    override def toString: String = schemaName
  }
  object SchemaRef {
    given Ordering[SchemaRef] = Ordering.by(_.schemaName)
  }

  final case class TableRef(schema: SchemaRef, tableName: String) extends EntityRef {
    override def toString: String = s"$schema.$tableName"
  }
  object TableRef {
    def apply(schemaName: String, tableName: String): TableRef = new TableRef(SchemaRef(schemaName), tableName)
    given Ordering[TableRef] = Ordering.by(r => (r.schema, r.tableName))
  }

  final case class ColumnRef(table: TableRef, columnName: String) extends EntityRef {
    override def toString: String = s"$table.$columnName"
  }
  object ColumnRef {
    def apply(schemaName: String, tableName: String, columnName: String): ColumnRef = new ColumnRef(TableRef(schemaName, tableName), columnName)
    given Ordering[ColumnRef] = Ordering.by(r => (r.table, r.columnName))
  }

}
