package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiff.*
import oxygen.sql.migration.model.StateDiffError.*
import oxygen.sql.migration.model.StateDiffError.Cause.*
import oxygen.sql.schema.*

final case class TableState private[migration] (
    tableName: TableRef,
    primaryKeyColumns: Contiguous[Column],
    columns: Contiguous[Column],
) {

  val pkColNames: Set[String] = primaryKeyColumns.map(_.name).toSet
  val colNames: Set[String] = columns.map(_.name).toSet

  /**
    * Returns a set of diffs that would transform `this` into `that`.
    */
  def diff(that: TableState): Either[DiffError, Seq[StateDiff.AlterColumn]] =
    for {
      _ <- Either.cond(this.primaryKeyColumns.toSet == that.primaryKeyColumns.toSet, (), DiffError(this.tableName, InvalidPrimaryKeyAlteration))
      thisMap = this.columns.map { c => (c.name, c) }.toMap
      thatMap = that.columns.map { c => (c.name, c) }.toMap

      diffs <- (thisMap.keySet | thatMap.keySet).toSeq
        .traverse { colName =>
          val colRef = ColumnRef(this.tableName, colName)
          (thisMap.get(colName), thatMap.get(colName)) match {
            case (Some(thisCol), Some(thatCol)) =>
              if (thisCol.columnType != thatCol.columnType) DiffError(colRef, SameNameDifferentType).asLeft
              else if (thisCol.nullable != thatCol.nullable) StateDiff.AlterColumn.SetNullable(colRef, thatCol.nullable).some.asRight
              else None.asRight
            case (Some(_), None) =>
              StateDiff.AlterColumn.DropColumn(colRef).some.asRight
            case (None, Some(thatCol)) =>
              StateDiff.AlterColumn.CreateColumn(this.tableName, thatCol).some.asRight
            case (None, None) =>
              None.asRight
          }
        }
        .map(_.flatten)
    } yield diffs

  private val unchanged: Either[Nothing, Option[TableState]] =
    this.some.asRight

  def apply(diff: StateDiff): Either[ApplyError, Option[TableState]] =
    diff match {
      case alterSchema: StateDiff.AlterSchema =>
        alterSchema match {
          case AlterSchema.CreateSchema(_) =>
            unchanged // NOTE : existence of schema needs to be validated elsewhere, schema with no tables can not have tables validate
          case AlterSchema.RenameSchema(schemaRef, newName) =>
            if (this.tableName.schema == schemaRef) copy(tableName = TableRef(newName, this.tableName.tableName)).some.asRight
            else unchanged
          case AlterSchema.DropSchema(schemaRef) =>
            if (this.tableName.schema == schemaRef) ApplyError(schemaRef, TableStillInSchema(this.tableName), diff).asLeft
            else unchanged
        }
      case alterTable: StateDiff.AlterTable =>
        alterTable match {
          case ct @ AlterTable.CreateTable(_) =>
            if (ct.tableRef == this.tableName) ApplyError(this.tableName, AlreadyExists, diff).asLeft
            else unchanged
          case AlterTable.RenameTable(tableRef, newName) =>
            if (tableRef == this.tableName) copy(tableName = TableRef(this.tableName.schema, newName)).some.asRight
            else unchanged
          case AlterTable.DropTable(tableRef) =>
            if (tableRef == this.tableName) None.asRight
            else unchanged
        }
      case alterColumn: StateDiff.AlterColumn =>
        alterColumn match {
          case cc @ AlterColumn.CreateColumn(tableRef, column) =>
            if (tableRef == this.tableName)
              if (colNames.contains(column.name)) ApplyError(cc.columnRef, AlreadyExists, diff).asLeft
              else copy(columns = this.columns :+ column).some.asRight
            else
              unchanged
          case AlterColumn.DropColumn(columnRef) =>
            if (columnRef.table == this.tableName)
              if (!colNames.contains(columnRef.columnName)) ApplyError(columnRef, DoesNotExist, diff).asLeft
              else if (pkColNames.contains(columnRef.columnName)) ApplyError(columnRef, InvalidPrimaryKeyAlteration, diff).asLeft
              else copy(columns = this.columns.filterNot(_.name == columnRef.columnName)).some.asRight
            else
              unchanged
          case AlterColumn.RenameColumn(columnRef, newName) =>
            if (columnRef.table == this.tableName)
              if (!colNames.contains(columnRef.columnName)) ApplyError(columnRef, DoesNotExist, diff).asLeft
              else if (pkColNames.contains(columnRef.columnName)) ApplyError(columnRef, InvalidPrimaryKeyAlteration, diff).asLeft
              else if (colNames.contains(newName)) ApplyError(columnRef, AlreadyExists, diff).asLeft
              else copy(columns = this.columns.filterNot(_.name == columnRef.columnName)).some.asRight
            else
              unchanged
          case AlterColumn.SetNullable(columnRef, nullable) =>
            if (columnRef.table == this.tableName)
              if (pkColNames.contains(columnRef.columnName)) ApplyError(columnRef, InvalidPrimaryKeyAlteration, diff).asLeft
              else
                columns.iterator.find(_.name == columnRef.columnName) match {
                  case Some(col) if col.nullable == nullable => ApplyError(columnRef, NullabilityNotChanged, diff).asLeft
                  case Some(_)                               => copy(columns = columns.map { c => if (c.name == columnRef.columnName) c.copy(nullable = nullable) else c }).some.asRight
                  case None                                  => ApplyError(columnRef, DoesNotExist, diff).asLeft
                }
            else
              unchanged
        }
    }

  def toIndentedString: IndentedString =
    IndentedString.section(s"table($tableName):")(
      IndentedString.section("primary-key-columns:")(primaryKeyColumns.map(_.toSql)),
      IndentedString.section("columns:")(columns.map(_.toSql)),
    )

}
object TableState {

  def fromTable(repr: TableRepr[?, ?]): Either[DeriveError, TableState] = {
    val ref = TableRef(repr.schemaName, repr.tableName)
    val cols: Contiguous[Column] = repr.rowRepr.columns.columns

    if (cols.length != cols.distinctBy(_.name).length)
      DeriveError(ref, NonDistinctColumnNames).asLeft
    else
      TableState(
        ref,
        repr.pk.rowRepr.columns.columns,
        cols,
      ).asRight
  }

  def unsafeFromTable(repr: TableRepr[?, ?]): TableState = fromTable(repr) match
    case Right(value) => value
    case Left(error)  => throw error

}
