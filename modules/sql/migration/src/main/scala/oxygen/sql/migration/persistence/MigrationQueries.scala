package oxygen.sql.migration.persistence

import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.StateDiff.*
import oxygen.sql.query.*

object MigrationQueries {

  def diffToQuery(diff: StateDiff): Query =
    diff match {
      case AlterSchema.CreateSchema(schemaRef) =>
        MigrationQueries.createSchema(schemaRef, false)
      case AlterSchema.RenameSchema(schemaRef, newName) =>
        Query.simple("Rename schema", QueryContext.QueryType.Migrate)(
          s"ALTER SCHEMA $schemaRef RENAME TO $newName",
        )
      case AlterSchema.DropSchema(schemaRef) =>
        Query.simple("Drop Schema", QueryContext.QueryType.Migrate)(
          s"DROP SCHEMA $schemaRef",
        )
      case AlterTable.CreateTable(table) =>
        MigrationQueries.createTable(table, false)
      case AlterTable.RenameTable(tableRef, newName) =>
        Query.simple("Rename Table", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE $tableRef RENAME TO $newName",
        )
      case AlterTable.DropTable(tableRef) =>
        Query.simple("Drop Table", QueryContext.QueryType.Migrate)(
          s"DROP TABLE $tableRef",
        )
      case AlterColumn.CreateColumn(tableRef, column) =>
        Query.simple("Add Column", QueryContext.QueryType.Migrate, "schema.table" -> tableRef.toString, "schema.column" -> column.name)(
          s"ALTER TABLE $tableRef ADD COLUMN ${column.toSql}",
        )
      case AlterColumn.DropColumn(columnRef) =>
        Query.simple("Drop Column", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE ${columnRef.table} DROP COLUMN ${columnRef.columnName}",
        )
      case AlterColumn.RenameColumn(columnRef, newName) =>
        Query.simple("Rename Column", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE ${columnRef.table} RENAME COLUMN ${columnRef.columnName} TO $newName",
        )
      case AlterColumn.SetNullable(columnRef, nullable) =>
        Query.simple("Set Nullability", QueryContext.QueryType.Migrate)(
          s"ALTER TABLE ${columnRef.table} ALTER COLUMN ${columnRef.columnName} ${if (nullable) "SET" else "DROP"} NOT NULL",
        )
      case AlterForeignKey.CreateForeignKey(fk) =>
        Query.simple("Create Foreign Key", QueryContext.QueryType.Migrate)(
          s"""ALTER TABLE ${fk.self}
             |    ADD CONSTRAINT ${fk.fkName}
             |    FOREIGN KEY (${fk.columnPairs.map(_.self).mkString(", ")})
             |    REFERENCES ${fk.references} (${fk.columnPairs.map(_.references).mkString(", ")})""".stripMargin,
        )
      case AlterForeignKey.RenameExplicitlyNamedForeignKey(fkRef, newName) =>
        Query.simple("Rename Foreign Key (explicit)", QueryContext.QueryType.Migrate)(
          s"""ALTER TABLE ${fkRef.table}
             |    RENAME CONSTRAINT ${fkRef.fkName}
             |    TO $newName""".stripMargin,
        )
      case AlterForeignKey.RenameAutoNamedForeignKey(fkRef, newName) =>
        Query.simple("Rename Foreign Key (auto)", QueryContext.QueryType.Migrate)(
          s"""ALTER TABLE ${fkRef.table}
             |    RENAME CONSTRAINT ${fkRef.fkName}
             |    TO $newName""".stripMargin,
        )
      case AlterForeignKey.DropForeignKey(fkRef) =>
        Query.simple("Drop Foreign Key", QueryContext.QueryType.Migrate)(
          s"""ALTER TABLE ${fkRef.table}
             |    DROP CONSTRAINT ${fkRef.fkName}""".stripMargin,
        )
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Table
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def createSchema(schema: EntityRef.SchemaRef, ifDNE: Boolean): Query =
    Query.simple("Create Schema", QueryContext.QueryType.Migrate)(
      s"CREATE SCHEMA${ifDNEStr(ifDNE)} $schema",
    )

  def createTable(table: TableState, ifDNE: Boolean): Query = {
    val columnLines: Seq[String] =
      table.columns.map { _.toSql }

    val constraintLines: Seq[String] =
      Seq[IterableOnce[String]](
        Option.when(table.primaryKeyColumns.nonEmpty) {
          s"PRIMARY KEY (${table.primaryKeyColumns.map(_.name).mkString(", ")})"
        },
        // not adding FKs here because then you need to worry about ordering...
      ).flatten

    val internalString: String =
      if (constraintLines.nonEmpty)
        columnLines.map { str => s"\n    ${str.replaceAll("\n", "\n    ")}," }.mkString +
          "\n" +
          constraintLines.map { str => s"\n    ${str.replaceAll("\n", "\n    ")}" }.mkString(",")
      else
        columnLines.map { str => s"\n    ${str.replaceAll("\n", "\n    ")}" }.mkString(",")

    Query.simple("Create Table", QueryContext.QueryType.Migrate)(
      s"CREATE TABLE${ifDNEStr(ifDNE)} ${table.tableName}($internalString\n)",
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def ifDNEStr(ifDNE: Boolean): String =
    if (ifDNE) " IF NOT EXISTS" else ""

}
