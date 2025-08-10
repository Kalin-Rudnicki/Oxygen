package oxygen.sql.migration.persistence.conversion

import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.model.*
import oxygen.sql.schema.Column

object dbToDomain {

  extension (self: EntityRefColumn.SchemaRef)
    def toDomain: EntityRef.SchemaRef =
      EntityRef.SchemaRef(self.schema)

  extension (self: EntityRefColumn.TableRef)
    def toDomain: EntityRef.TableRef =
      EntityRef.TableRef(self.schema, self.table)

  extension (self: EntityRefColumn.ColumnRef)
    def toDomain: EntityRef.ColumnRef =
      EntityRef.ColumnRef(self.schema, self.table, self.column)

  extension (self: ColumnColumn)
    def toDomain: Column =
      Column(
        name = self.name,
        columnType = self.columnType.toDomain,
        nullable = self.nullable,
      )

  extension (self: ColumnColumn.Type)
    def toDomain: Column.Type = self match
      case ColumnColumn.Type.SmallInt        => Column.Type.SmallInt
      case ColumnColumn.Type.Int             => Column.Type.Int
      case ColumnColumn.Type.BigInt          => Column.Type.BigInt
      case ColumnColumn.Type.Real            => Column.Type.Real
      case ColumnColumn.Type.DoublePrecision => Column.Type.DoublePrecision
      case ColumnColumn.Type.Text            => Column.Type.Text
      case ColumnColumn.Type.Timestamp       => Column.Type.Timestamp
      case ColumnColumn.Type.ZonedTimestamp  => Column.Type.ZonedTimestamp
      case ColumnColumn.Type.Date            => Column.Type.Date
      case ColumnColumn.Type.Time            => Column.Type.Time
      case ColumnColumn.Type.Boolean         => Column.Type.Boolean
      case ColumnColumn.Type.UUID            => Column.Type.UUID
      case ColumnColumn.Type.Json            => Column.Type.Json
      case ColumnColumn.Type.Jsonb           => Column.Type.Jsonb

  extension (self: TableStateColumn)
    def toDomain: TableState = {
      val cols = self.columns.map(_.toDomain)
      TableState(
        tableName = self.tableName.toDomain,
        primaryKeyColumns = cols.filter(c => self.primaryKeyColumns.contains(c.name)),
        columns = cols,
      )
    }

  extension (self: MigrationStepColumn.StateDiff)
    def toDomain: StateDiff = self match
      case MigrationStepColumn.AlterSchema.CreateSchema(schemaRef)          => StateDiff.AlterSchema.CreateSchema(schemaRef.toDomain)
      case MigrationStepColumn.AlterSchema.RenameSchema(schemaRef, newName) => StateDiff.AlterSchema.RenameSchema(schemaRef.toDomain, newName)
      case MigrationStepColumn.AlterSchema.DropSchema(schemaRef)            => StateDiff.AlterSchema.DropSchema(schemaRef.toDomain)
      case MigrationStepColumn.AlterTable.CreateTable(table)                => StateDiff.AlterTable.CreateTable(table.toDomain)
      case MigrationStepColumn.AlterTable.RenameTable(tableRef, newName)    => StateDiff.AlterTable.RenameTable(tableRef.toDomain, newName)
      case MigrationStepColumn.AlterTable.DropTable(tableRef)               => StateDiff.AlterTable.DropTable(tableRef.toDomain)
      case MigrationStepColumn.AlterColumn.CreateColumn(tableRef, column)   => StateDiff.AlterColumn.CreateColumn(tableRef.toDomain, column.toDomain)
      case MigrationStepColumn.AlterColumn.DropColumn(columnRef)            => StateDiff.AlterColumn.DropColumn(columnRef.toDomain)
      case MigrationStepColumn.AlterColumn.RenameColumn(columnRef, newName) => StateDiff.AlterColumn.RenameColumn(columnRef.toDomain, newName)
      case MigrationStepColumn.AlterColumn.SetNullable(columnRef, nullable) => StateDiff.AlterColumn.SetNullable(columnRef.toDomain, nullable)

  extension (self: ExecutedMigrationStepRow)
    def toDomain: ExecutedMigration.Step =
      ExecutedMigration.Step(
        stepNo = self.stepNo,
        derived = self.derived,
        step = self.step.value match {
          case diff: MigrationStepColumn.StateDiff => ExecutedMigration.StepType.Diff(diff.toDomain)
        },
        sql = self.sql,
      )

}
