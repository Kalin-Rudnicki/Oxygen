package oxygen.sql.migration.persistence.conversion

import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.model.*
import oxygen.sql.schema.Column

object domainToDb {

  extension (self: EntityRef.SchemaRef)
    def toDb: EntityRefColumn.SchemaRef =
      EntityRefColumn.SchemaRef(self.schemaName)

  extension (self: EntityRef.TableRef)
    def toDb: EntityRefColumn.TableRef =
      EntityRefColumn.TableRef(self.schema.schemaName, self.tableName)

  extension (self: EntityRef.ColumnRef)
    def toDb: EntityRefColumn.ColumnRef =
      EntityRefColumn.ColumnRef(self.table.schema.schemaName, self.table.tableName, self.columnName)

  extension (self: Column)
    def toDb: ColumnColumn =
      ColumnColumn(
        name = self.name,
        columnType = self.columnType.toDb,
        nullable = self.nullable,
      )

  extension (self: Column.Type)
    def toDb: ColumnColumn.Type = self match
      case Column.Type.SmallInt        => ColumnColumn.Type.SmallInt
      case Column.Type.Int             => ColumnColumn.Type.Int
      case Column.Type.BigInt          => ColumnColumn.Type.BigInt
      case Column.Type.Real            => ColumnColumn.Type.Real
      case Column.Type.DoublePrecision => ColumnColumn.Type.DoublePrecision
      case Column.Type.Text            => ColumnColumn.Type.Text
      case Column.Type.Timestamp       => ColumnColumn.Type.Timestamp
      case Column.Type.ZonedTimestamp  => ColumnColumn.Type.ZonedTimestamp
      case Column.Type.Date            => ColumnColumn.Type.Date
      case Column.Type.Time            => ColumnColumn.Type.Time
      case Column.Type.Boolean         => ColumnColumn.Type.Boolean
      case Column.Type.UUID            => ColumnColumn.Type.UUID
      case Column.Type.Json            => ColumnColumn.Type.Json
      case Column.Type.Jsonb           => ColumnColumn.Type.Jsonb

  extension (self: TableState)
    def toDb: TableStateColumn =
      TableStateColumn(
        tableName = self.tableName.toDb,
        primaryKeyColumns = self.primaryKeyColumns.iterator.map(_.name).toSet,
        columns = self.columns.map(_.toDb),
      )

  extension (self: StateDiff)
    def toDb: MigrationStepColumn.StateDiff = self match
      case StateDiff.AlterSchema.CreateSchema(schemaRef)          => MigrationStepColumn.AlterSchema.CreateSchema(schemaRef.toDb)
      case StateDiff.AlterSchema.RenameSchema(schemaRef, newName) => MigrationStepColumn.AlterSchema.RenameSchema(schemaRef.toDb, newName)
      case StateDiff.AlterSchema.DropSchema(schemaRef)            => MigrationStepColumn.AlterSchema.DropSchema(schemaRef.toDb)
      case StateDiff.AlterTable.CreateTable(table)                => MigrationStepColumn.AlterTable.CreateTable(table.toDb)
      case StateDiff.AlterTable.RenameTable(tableRef, newName)    => MigrationStepColumn.AlterTable.RenameTable(tableRef.toDb, newName)
      case StateDiff.AlterTable.DropTable(tableRef)               => MigrationStepColumn.AlterTable.DropTable(tableRef.toDb)
      case StateDiff.AlterColumn.CreateColumn(tableRef, column)   => MigrationStepColumn.AlterColumn.CreateColumn(tableRef.toDb, column.toDb)
      case StateDiff.AlterColumn.DropColumn(columnRef)            => MigrationStepColumn.AlterColumn.DropColumn(columnRef.toDb)
      case StateDiff.AlterColumn.RenameColumn(columnRef, newName) => MigrationStepColumn.AlterColumn.RenameColumn(columnRef.toDb, newName)
      case StateDiff.AlterColumn.SetNullable(columnRef, nullable) => MigrationStepColumn.AlterColumn.SetNullable(columnRef.toDb, nullable)

}
