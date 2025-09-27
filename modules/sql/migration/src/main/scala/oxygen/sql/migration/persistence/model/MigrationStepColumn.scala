package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import oxygen.predef.core.*
import oxygen.sql.migration.persistence.model.EntityRefColumn.*

sealed trait MigrationStepColumn derives JsonCodec
object MigrationStepColumn {

  sealed trait StateDiff extends MigrationStepColumn

  sealed trait AlterSchema extends StateDiff
  object AlterSchema {

    final case class CreateSchema(schemaRef: SchemaRef) extends AlterSchema

    final case class RenameSchema(schemaRef: SchemaRef, newName: String) extends AlterSchema

    final case class DropSchema(schemaRef: SchemaRef) extends AlterSchema

  }

  sealed trait AlterTable extends StateDiff
  object AlterTable {

    final case class CreateTable(table: TableStateColumn) extends AlterTable

    final case class RenameTable(tableRef: TableRef, newName: String) extends AlterTable

    final case class DropTable(tableRef: TableRef) extends AlterTable

  }

  sealed trait AlterColumn extends StateDiff
  object AlterColumn {

    final case class CreateColumn(tableRef: TableRef, column: ColumnColumn) extends AlterColumn

    final case class DropColumn(columnRef: ColumnRef) extends AlterColumn

    final case class RenameColumn(columnRef: ColumnRef, newName: String) extends AlterColumn

    final case class SetNullable(columnRef: ColumnRef, nullable: Boolean) extends AlterColumn

  }

  sealed trait AlterForeignKey extends StateDiff
  object AlterForeignKey {

    final case class CreateForeignKey(fk: MigrationForeignKeyColumn) extends AlterForeignKey

    final case class RenameExplicitlyNamedForeignKey(fkRef: ForeignKeyRef, newName: String) extends AlterForeignKey

    final case class RenameAutoNamedForeignKey(fkRef: ForeignKeyRef, newName: String) extends AlterForeignKey

    final case class DropForeignKey(fkRef: ForeignKeyRef) extends AlterForeignKey

  }

  sealed trait AlterIndex extends StateDiff
  object AlterIndex {

    final case class CreateIndex(idx: IndexColumn) extends AlterIndex

    final case class RenameExplicitlyNamedIndex(idxRef: IndexRef, newName: String) extends AlterIndex

    final case class RenameAutoNamedIndex(idxRef: IndexRef, newName: String) extends AlterIndex

    final case class DropIndex(idxRef: IndexRef) extends AlterIndex

  }

}
