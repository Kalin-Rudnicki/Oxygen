package oxygen.sql.migration.model

import oxygen.core.generic.*
import oxygen.predef.core.*

sealed trait EntityRef derives Show
object EntityRef {

  final case class SchemaRef(schemaName: String) extends EntityRef {
    def isPublic: Boolean = schemaName == "public"
    override def toString: String = schemaName
  }
  object SchemaRef {
    given Ordering[SchemaRef] = Ordering.by(_.schemaName)
    given Show[SchemaRef] = v => s"SchemaRef(schemaName = ${v.schemaName})"
  }

  final case class TableRef(schema: SchemaRef, tableName: String) extends EntityRef {
    override def toString: String = s"$schema.$tableName"
  }
  object TableRef {
    def apply(schemaName: String, tableName: String): TableRef = new TableRef(SchemaRef(schemaName), tableName)
    given Ordering[TableRef] = Ordering.by(r => (r.schema, r.tableName))
    given Show[TableRef] = v => s"TableRef(schemaName = ${v.schema.schemaName}, tableName = ${v.tableName})"
  }

  final case class ColumnRef(table: TableRef, columnName: String) extends EntityRef {
    override def toString: String = s"$table.$columnName"
  }
  object ColumnRef {
    def apply(schemaName: String, tableName: String, columnName: String): ColumnRef = new ColumnRef(TableRef(schemaName, tableName), columnName)
    given Ordering[ColumnRef] = Ordering.by(r => (r.table, r.columnName))
    given Show[ColumnRef] = v => s"ColumnRef(schemaName = ${v.table.schema.schemaName}, tableName = ${v.table.tableName}, columnName = ${v.columnName})"
  }

  final case class ForeignKeyRef(table: TableRef, fkName: String) extends EntityRef derives Show {
    override def toString: String = s"$table ($fkName)"
  }
  object ForeignKeyRef {
    def apply(schemaName: String, tableName: String, fkName: String): ForeignKeyRef = new ForeignKeyRef(TableRef(schemaName, tableName), fkName)
    given Ordering[ForeignKeyRef] = Ordering.by(r => (r.table, r.fkName))
  }

}
