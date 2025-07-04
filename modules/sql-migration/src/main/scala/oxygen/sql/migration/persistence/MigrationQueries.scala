package oxygen.sql.migration.persistence

import oxygen.core.collection.Contiguous
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.StateDiff.*
import oxygen.sql.query.*

object MigrationQueries {

  def diffToQuery(diff: StateDiff): Query =
    diff match {
      case AlterSchema.CreateSchema(schemaRef) =>
        MigrationQueries.createSchema(schemaRef, false)
      case AlterSchema.RenameSchema(schemaRef, newName) =>
        Query.simple(s"Rename schema : $schemaRef -> $newName", QueryContext.QueryType.Migrate)(
          s"ALTER SCHEMA $schemaRef RENAME TO $newName",
        )
      case AlterSchema.DropSchema(schemaRef) =>
        Query.simple(s"Drop Schema : $schemaRef", QueryContext.QueryType.Migrate)(
          s"DROP SCHEMA $schemaRef",
        )
      case AlterTable.CreateTable(table) =>
        MigrationQueries.createTable(table, false)
      case AlterTable.RenameTable(tableRef, newName) =>
        Query.simple(s"Rename Table : $tableRef -> $newName", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE $tableRef RENAME TO $newName",
        )
      case AlterTable.DropTable(tableRef) =>
        Query.simple(s"Drop Table : $tableRef", QueryContext.QueryType.Migrate)(
          s"DROP TABLE $tableRef",
        )
      case AlterColumn.CreateColumn(tableRef, column) =>
        Query.simple(s"Add Column : $tableRef.${column.name}", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE $tableRef ADD COLUMN ${column.toSql}",
        )
      case AlterColumn.DropColumn(columnRef) =>
        Query.simple(s"Drop Column : $columnRef", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE ${columnRef.table} DROP COLUMN ${columnRef.columnName}",
        )
      case AlterColumn.RenameColumn(columnRef, newName) =>
        Query.simple(s"Rename Column : $columnRef -> $newName", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE ${columnRef.table} RENAME COLUMN ${columnRef.columnName} TO $newName",
        )
      case AlterColumn.SetNullable(columnRef, nullable) =>
        Query.simple(s"Set Nullability : $columnRef, nullable = $nullable", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE ${columnRef.table} ALTER COLUMN ${columnRef.columnName} ${if (nullable) "SET" else "DROP"} NOT NULL",
        )
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Table
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def createSchema(schema: EntityRef.SchemaRef, ifDNE: Boolean): Query =
    Query.simple(s"Create Schema : $schema", QueryContext.QueryType.Migrate)(
      s"CREATE SCHEMA${ifDNEStr(ifDNE)} $schema",
    )

  def createTable(table: TableState, ifDNE: Boolean): Query = {
    val columnLines: Contiguous[String] =
      table.columns.map { c => s"\n    ${c.toSql}" }
    val pkLine: String =
      if (table.primaryKeyColumns.nonEmpty) s",\n\n    PRIMARY KEY (${table.primaryKeyColumns.map(_.name).mkString(", ")})"
      else ""

    Query.simple(s"Create Table : ${table.tableName}", QueryContext.QueryType.Migrate)(
      s"CREATE TABLE${ifDNEStr(ifDNE)} ${table.tableName}(${columnLines.mkString(", ")}$pkLine\n)",
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def ifDNEStr(ifDNE: Boolean): String =
    if (ifDNE) " IF NOT EXISTS" else ""

}
