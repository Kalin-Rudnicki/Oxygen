package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiff.*
import oxygen.sql.schema.*

sealed trait StateDiff {

  val applicationOrder: Int = this match
    case _: AlterSchema.CreateSchema => 1
    case _: AlterSchema.RenameSchema => 2
    case _: AlterTable.CreateTable   => 3
    case _: AlterTable.RenameTable   => 4
    case _: AlterColumn.CreateColumn => 5
    case _: AlterColumn.RenameColumn => 6
    case _: AlterColumn.SetNullable  => 7
    case _: AlterColumn.DropColumn   => 8
    case _: AlterTable.DropTable     => 9
    case _: AlterSchema.DropSchema   => 10

  // TODO (KR) :
  final def toIndentedString: IndentedString =
    this.toString

}
object StateDiff {

  sealed trait AlterSchema extends StateDiff {
    val schemaRef: SchemaRef
  }
  object AlterSchema {

    final case class CreateSchema(schemaRef: SchemaRef) extends AlterSchema

    final case class RenameSchema(schemaRef: SchemaRef, newName: String) extends AlterSchema

    final case class DropSchema(schemaRef: SchemaRef) extends AlterSchema

  }

  sealed trait AlterTable extends StateDiff {
    val tableRef: TableRef
  }
  object AlterTable {

    final case class CreateTable(table: TableState) extends AlterTable {
      override val tableRef: EntityRef.TableRef = table.tableName
    }

    final case class RenameTable(tableRef: TableRef, newName: String) extends AlterTable

    final case class DropTable(tableRef: TableRef) extends AlterTable

  }

  sealed trait AlterColumn extends StateDiff {
    val columnRef: ColumnRef
  }
  object AlterColumn {

    final case class CreateColumn(tableRef: TableRef, column: Column) extends AlterColumn {
      override val columnRef: ColumnRef = ColumnRef(tableRef, column.name)
    }

    final case class DropColumn(columnRef: ColumnRef) extends AlterColumn

    final case class RenameColumn(columnRef: ColumnRef, newName: String) extends AlterColumn

    final case class SetNullable(columnRef: ColumnRef, nullable: Boolean) extends AlterColumn

  }

}
