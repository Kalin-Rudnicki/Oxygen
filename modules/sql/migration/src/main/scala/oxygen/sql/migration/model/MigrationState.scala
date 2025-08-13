package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiff.*
import oxygen.sql.migration.model.StateDiffError.*
import oxygen.sql.migration.model.StateDiffError.Cause.*
import oxygen.sql.schema.TableRepr

final case class MigrationState private (
    schemas: Set[SchemaRef],
    tables: Map[TableRef, TableState],
) {

  /**
    * Returns a set of diffs that would transform `this` into `that`.
    */
  def diff(that: MigrationState): Either[DiffError, ArraySeq[StateDiff]] = {
    val createSchemas: Growable[StateDiff] = Growable.many(that.schemas &~ this.schemas: Iterable[SchemaRef]).filterNot(_.schemaName == "public").map { StateDiff.AlterSchema.CreateSchema(_) }
    val deleteSchemas: Growable[StateDiff] = Growable.many(this.schemas &~ that.schemas: Iterable[SchemaRef]).filterNot(_.schemaName == "public").map { StateDiff.AlterSchema.DropSchema(_) }
    val alterTables: Either[DiffError, Growable[StateDiff]] =
      Growable
        .many(this.tables.keySet | that.tables.keySet: Iterable[TableRef])
        .traverse { tableName =>
          (this.tables.get(tableName), that.tables.get(tableName)) match {
            case (Some(thisTable), Some(thatTable)) => thisTable.diff(thatTable).map(Growable.many)
            case (Some(thisTable), None)            => Growable(StateDiff.AlterTable.DropTable(thisTable.tableName)).asRight
            case (None, Some(thatTable))            => Growable(StateDiff.AlterTable.CreateTable(thatTable)).asRight
            case (None, None)                       => Growable.empty[StateDiff].asRight
          }
        }
        .map(_.flatten)

    alterTables.map { _ ++ createSchemas ++ deleteSchemas }.map(_.toArraySeq.sortBy(_.applicationOrder))
  }

  private def modifyTables(diff: StateDiff): Either[ApplyError, MigrationState] =
    tables.values.toSeq.traverse(_.apply(diff)).map(_.flatten).map { newTables => copy(tables = newTables.map(t => (t.tableName, t)).toMap) }

  def apply(diff: StateDiff): Either[ApplyError, MigrationState] =
    diff match {
      case alterSchema: StateDiff.AlterSchema =>
        alterSchema match {
          case AlterSchema.CreateSchema(schemaRef) =>
            if (schemas.contains(schemaRef)) ApplyError(schemaRef, AlreadyExists, diff).asLeft
            else copy(schemas = schemas + schemaRef).asRight
          case AlterSchema.RenameSchema(schemaRef, newName) =>
            if (!schemas.contains(schemaRef)) ApplyError(schemaRef, DoesNotExist, diff).asLeft
            else if (schemas.contains(SchemaRef(newName))) ApplyError(SchemaRef(newName), AlreadyExists, diff).asLeft
            else copy(schemas = schemas.map { s => if (s.schemaName == schemaRef.schemaName) SchemaRef(newName) else s }).modifyTables(diff)
          case AlterSchema.DropSchema(schemaRef) =>
            if (!schemas.contains(schemaRef)) ApplyError(schemaRef, DoesNotExist, diff).asLeft
            else copy(schemas = schemas - schemaRef).modifyTables(diff)
        }
      case alterTable: StateDiff.AlterTable =>
        alterTable match {
          case ct @ AlterTable.CreateTable(ts) =>
            if (!schemas.contains(ct.tableRef.schema)) ApplyError(ct.tableRef.schema, DoesNotExist, diff).asLeft
            else if (tables.contains(ct.tableRef)) ApplyError(ct.tableRef, AlreadyExists, diff).asLeft
            else
              for {
                updated <- modifyTables(diff)
              } yield updated.copy(tables = updated.tables.updated(ts.tableName, ts))
          case AlterTable.RenameTable(tableRef, newName) =>
            if (!tables.contains(tableRef)) ApplyError(tableRef, DoesNotExist, diff).asLeft
            else if (tables.contains(TableRef(tableRef.schema, newName))) ApplyError(TableRef(tableRef.schema, newName), AlreadyExists, diff).asLeft
            else modifyTables(diff)
          case AlterTable.DropTable(tableRef) =>
            if (!tables.contains(tableRef)) ApplyError(tableRef, DoesNotExist, diff).asLeft
            else modifyTables(diff)
        }
      case alterColumn: StateDiff.AlterColumn =>
        if (!tables.contains(alterColumn.columnRef.table)) ApplyError(alterColumn.columnRef.table, DoesNotExist, diff).asLeft
        else modifyTables(diff)
    }

  def applyAll(diffs: ArraySeq[StateDiff]): Either[ApplyError, MigrationState] =
    diffs.eitherFoldLeft(this)(_.apply(_))

  def toIndentedString: IndentedString =
    IndentedString.section("Migration State:")(
      IndentedString.section("schemas:")(schemas.toSeq.sorted.map(_.toString)),
      IndentedString.section("tables:")(tables.values.toSeq.sortBy(_.tableName).map(_.toIndentedString)),
    )

}
object MigrationState {

  val empty: MigrationState = MigrationState(Set(SchemaRef("public")), Map.empty)

  def fromTables(schemas: ArraySeq[TableRepr[?]]): Either[DeriveError, MigrationState] =
    schemas.traverse(TableState.fromTable).map { tables =>
      MigrationState(tables.map(_.tableName.schema).toSet + SchemaRef("public"), tables.map(t => (t.tableName, t)).toMap)
    }

}
