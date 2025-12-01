package oxygen.sql.migration.delta

import oxygen.predef.core.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiffError.ApplyError
import oxygen.sql.migration.model.StateDiffError.Cause.*

object DiffApplier {

  def applyOne(
      state0: MigrationState,
      diff: StateDiff,
  ): Either[ApplyError, MigrationState] =
    for {
      state1 <- modifyMigrationState(state0, diff)
      state2 <- modifyTables(state1, diff)
    } yield state2

  private def modifyMigrationState(
      currentState: MigrationState,
      diff: StateDiff,
  ): Either[ApplyError, MigrationState] =
    diff match {
      case diff: StateDiff.DerivationPhase.Phase1 =>
        diff match {
          case StateDiff.AlterSchema.CreateSchema(schemaRef) =>
            if currentState.schemas.contains(schemaRef) then ApplyError(schemaRef, AlreadyExists, diff).asLeft
            else currentState.copy(schemas = currentState.schemas + schemaRef).asRight
          case StateDiff.AlterSchema.DropSchema(schemaRef) =>
            if !currentState.schemas.contains(schemaRef) then ApplyError(schemaRef, DoesNotExist, diff).asLeft
            else currentState.copy(schemas = currentState.schemas - schemaRef).asRight
          case StateDiff.AlterTable.CreateTable(tableState) =>
            if !currentState.schemas.contains(tableState.tableName.schema) then ApplyError(tableState.tableName.schema, DoesNotExist, diff).asLeft
            else if currentState.tables.contains(tableState.tableName) then ApplyError(tableState.tableName, AlreadyExists, diff).asLeft
            else if tableState.foreignKeys.nonEmpty then ApplyError(tableState.tableName, CreateTableWithFK, diff).asLeft
            else if tableState.indices.nonEmpty then ApplyError(tableState.tableName, CreateTableWithIdx, diff).asLeft
            else currentState.copy(tables = currentState.tables.updated(tableState.tableName, tableState)).asRight
          case StateDiff.AlterTable.DropTable(tableRef) =>
            if !currentState.tables.contains(tableRef) then ApplyError(tableRef, DoesNotExist, diff).asLeft
            else currentState.copy(tables = currentState.tables.removed(tableRef)).asRight
          case StateDiff.AlterColumn.CreateColumn(tableRef, _) =>
            if !currentState.tables.contains(tableRef) then ApplyError(tableRef, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterColumn.DropColumn(columnRef) =>
            if !currentState.tables.contains(columnRef.table) then ApplyError(columnRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterColumn.SetNullable(columnRef, _) =>
            if !currentState.tables.contains(columnRef.table) then ApplyError(columnRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
        }
      case diff: StateDiff.DerivationPhase.Phase2 =>
        diff match {
          case StateDiff.AlterForeignKey.CreateForeignKey(fk) =>
            if !currentState.tables.contains(fk.self) then ApplyError(fk.self, DoesNotExist, diff).asLeft
            if !currentState.tables.contains(fk.references) then ApplyError(fk.references, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterForeignKey.DropForeignKey(fkRef) =>
            if !currentState.tables.contains(fkRef.table) then ApplyError(fkRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterIndex.CreateIndex(idx) =>
            if !currentState.tables.contains(idx.self) then ApplyError(idx.self, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterIndex.DropIndex(idxRef) =>
            if !currentState.tables.contains(idxRef.table) then ApplyError(idxRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
        }
      case diff: StateDiff.DerivationPhase.Phase3 =>
        diff match {
          case StateDiff.AlterForeignKey.RenameAutoNamedForeignKey(fkRef, _) =>
            if !currentState.tables.contains(fkRef.table) then ApplyError(fkRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterIndex.RenameAutoNamedIndex(idxRef, _) =>
            if !currentState.tables.contains(idxRef.table) then ApplyError(idxRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
        }
      case diff: StateDiff.CanOnlyBeSpecified =>
        diff match {
          case StateDiff.AlterSchema.RenameSchema(schemaRef, newName) =>
            val newSchemaName = SchemaRef(newName)
            if !currentState.schemas.contains(schemaRef) then ApplyError(schemaRef, DoesNotExist, diff).asLeft
            else if currentState.schemas.contains(newSchemaName) then ApplyError(newSchemaName, AlreadyExists, diff).asLeft
            else if schemaRef.isPublic then ApplyError(schemaRef, ModificationToPublicSchema, diff).asLeft
            else if newSchemaName.isPublic then ApplyError(newSchemaName, ModificationToPublicSchema, diff).asLeft
            else currentState.copy(schemas = currentState.schemas - schemaRef + newSchemaName).asRight
          case StateDiff.AlterTable.RenameTable(tableRef, _) =>
            if !currentState.tables.contains(tableRef) then ApplyError(tableRef, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterColumn.RenameColumn(columnRef, _) =>
            if !currentState.tables.contains(columnRef.table) then ApplyError(columnRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterForeignKey.RenameExplicitlyNamedForeignKey(fkRef, _) =>
            if !currentState.tables.contains(fkRef.table) then ApplyError(fkRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
          case StateDiff.AlterIndex.RenameExplicitlyNamedIndex(idxRef, _) =>
            if !currentState.tables.contains(idxRef.table) then ApplyError(idxRef.table, DoesNotExist, diff).asLeft
            else currentState.asRight
        }
    }

  private def modifyTableState(
      currentState: TableState,
      diff: StateDiff,
  ): Either[StateDiffError.ApplyError, Option[TableState]] =
    diff match {
      case diff: StateDiff.DerivationPhase.Phase1 =>
        diff match {
          case _: StateDiff.AlterSchema.CreateSchema    => currentState.some.asRight
          case _: StateDiff.AlterSchema.DropSchema      => currentState.some.asRight
          case _: StateDiff.AlterTable.CreateTable      => currentState.some.asRight
          case StateDiff.AlterTable.DropTable(tableRef) =>
            if currentState.tableName == tableRef then None.asRight
            else currentState.some.asRight
          case StateDiff.AlterColumn.CreateColumn(tableRef, column) =>
            if currentState.tableName != tableRef then currentState.some.asRight
            else if currentState.columns.exists(_.name == column.name) then ApplyError(ColumnRef(tableRef, column.name), AlreadyExists, diff).asLeft
            else currentState.copy(columns = currentState.columns :+ column).some.asRight
          case StateDiff.AlterColumn.DropColumn(columnRef) =>
            if currentState.tableName != columnRef.table then currentState.some.asRight
            else if !currentState.columns.exists(_.name == columnRef.columnName) then ApplyError(columnRef, DoesNotExist, diff).asLeft
            else if currentState.primaryKeyColumns.exists(_.name == columnRef.columnName) then ApplyError(columnRef, InvalidPrimaryKeyAlteration, diff).asLeft
            else currentState.copy(columns = currentState.columns.filterNot(_.name == columnRef.columnName)).some.asRight
          case StateDiff.AlterColumn.SetNullable(columnRef, nullable) =>
            if currentState.tableName != columnRef.table then currentState.some.asRight
            else if !currentState.columns.exists(_.name == columnRef.columnName) then ApplyError(columnRef, DoesNotExist, diff).asLeft
            else if currentState.primaryKeyColumns.exists(_.name == columnRef.columnName) then ApplyError(columnRef, InvalidPrimaryKeyAlteration, diff).asLeft
            else
              currentState.columns
                .traverse { c =>
                  if c.name != columnRef.columnName then c.asRight
                  else if c.nullable == nullable then ApplyError(columnRef, NullabilityNotChanged, diff).asLeft
                  else c.copy(nullable = nullable).asRight
                }
                .map { newCols => currentState.copy(columns = newCols).some }
        }
      case diff: StateDiff.DerivationPhase.Phase2 =>
        diff match {
          case StateDiff.AlterForeignKey.CreateForeignKey(fk) =>
            if currentState.tableName != fk.self then currentState.some.asRight
            else if currentState.foreignKeys.exists(_.ref == fk.ref) then ApplyError(fk.ref, AlreadyExists, diff).asLeft
            else currentState.copy(foreignKeys = currentState.foreignKeys :+ fk).some.asRight
          case StateDiff.AlterForeignKey.DropForeignKey(fkRef) =>
            if currentState.tableName != fkRef.table then currentState.some.asRight
            else if !currentState.foreignKeys.exists(_.ref == fkRef) then ApplyError(fkRef, DoesNotExist, diff).asLeft
            else currentState.copy(foreignKeys = currentState.foreignKeys.filterNot(_.ref == fkRef)).some.asRight
          case StateDiff.AlterIndex.CreateIndex(idx) =>
            if currentState.tableName != idx.self then currentState.some.asRight
            else if currentState.indices.exists(_.ref == idx.ref) then ApplyError(idx.ref, AlreadyExists, diff).asLeft
            else currentState.copy(indices = currentState.indices :+ idx).some.asRight
          case StateDiff.AlterIndex.DropIndex(idxRef) =>
            if currentState.tableName != idxRef.table then currentState.some.asRight
            else if !currentState.indices.exists(_.ref == idxRef) then ApplyError(idxRef, DoesNotExist, diff).asLeft
            else currentState.copy(indices = currentState.indices.filterNot(_.ref == idxRef)).some.asRight
        }
      case diff: StateDiff.DerivationPhase.Phase3 =>
        diff match {
          case _: StateDiff.AlterForeignKey.RenameAutoNamedForeignKey => currentState.some.asRight // this is updated in the renames
          case _: StateDiff.AlterIndex.RenameAutoNamedIndex           => currentState.some.asRight // this is updated in the renames
        }
      case diff: StateDiff.CanOnlyBeSpecified =>
        diff match {
          case StateDiff.AlterSchema.RenameSchema(schemaRef, newName) =>
            currentState
              .copy(
                tableName = if currentState.tableName.schema == schemaRef then TableRef(newName, currentState.tableName.tableName) else currentState.tableName,
                foreignKeys = currentState.foreignKeys.map(_.renameSchema(schemaRef, newName)),
                indices = currentState.indices.map(_.renameSchema(schemaRef, newName)),
              )
              .some
              .asRight
          case StateDiff.AlterTable.RenameTable(tableRef, newName) =>
            currentState
              .copy(
                tableName = if currentState.tableName == tableRef then TableRef(currentState.tableName.schema, newName) else currentState.tableName,
                foreignKeys = currentState.foreignKeys.map(_.renameTable(tableRef, newName)),
                indices = currentState.indices.map(_.renameTable(tableRef, newName)),
              )
              .some
              .asRight
          case StateDiff.AlterColumn.RenameColumn(columnRef, newName) =>
            currentState
              .copy(
                columns =
                  if currentState.tableName == columnRef.table then currentState.columns.map { c => if c.name == columnRef.columnName then c.copy(name = newName) else c } else currentState.columns,
                primaryKeyColumns = if currentState.tableName == columnRef.table then currentState.primaryKeyColumns.map { c => if c.name == columnRef.columnName then c.copy(name = newName) else c }
                else currentState.primaryKeyColumns,
                foreignKeys = currentState.foreignKeys.map(_.renameColumn(columnRef, newName)),
                indices = currentState.indices.map(_.renameColumn(columnRef, newName)),
              )
              .some
              .asRight
          case StateDiff.AlterForeignKey.RenameExplicitlyNamedForeignKey(fkRef, newName) =>
            if currentState.tableName != fkRef.table then currentState.some.asRight
            else if !currentState.foreignKeys.exists { fk => fk.ref == fkRef && fk.explicitFKName.nonEmpty } then ApplyError(fkRef, DoesNotExist, diff).asLeft
            else if currentState.foreignKeys.exists(_.fkName == newName) then ApplyError(fkRef.copy(fkName = newName), AlreadyExists, diff).asLeft
            else currentState.copy(foreignKeys = currentState.foreignKeys.map { fk => if fk.explicitFKName.contains(fkRef.fkName) then fk.copy(explicitFKName = newName.some) else fk }).some.asRight
          case StateDiff.AlterIndex.RenameExplicitlyNamedIndex(idxRef, newName) =>
            if currentState.tableName != idxRef.table then currentState.some.asRight
            else if !currentState.indices.exists { idx => idx.ref == idxRef && idx.explicitIdxName.nonEmpty } then ApplyError(idxRef, DoesNotExist, diff).asLeft
            else if currentState.indices.exists(_.idxName == newName) then ApplyError(idxRef.copy(idxName = newName), AlreadyExists, diff).asLeft
            else currentState.copy(indices = currentState.indices.map { idx => if idx.explicitIdxName.contains(idxRef.idxName) then idx.copy(explicitIdxName = newName.some) else idx }).some.asRight
        }
    }

  private def modifyTables(currentState: MigrationState, diff: StateDiff): Either[ApplyError, MigrationState] =
    currentState.tables.values.toSeq.traverse(modifyTableState(_, diff)).map(_.flatten).map { newTables => currentState.copy(tables = newTables.map(t => (t.tableName, t)).toMap) }

  def applyAll(
      currentState: MigrationState,
      diffs: ArraySeq[StateDiff],
  ): Either[StateDiffError.ApplyError, MigrationState] =
    diffs.eitherFoldLeft(currentState)(applyOne(_, _))

}
