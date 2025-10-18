package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiffError.*
import oxygen.sql.migration.model.StateDiffError.Cause.*
import oxygen.sql.schema.*

final case class TableState private[migration] (
    tableName: TableRef,
    primaryKeyColumns: ArraySeq[Column],
    columns: ArraySeq[Column],
    foreignKeys: ArraySeq[ForeignKeyState],
    indices: ArraySeq[IndexState],
) derives Show {

  val pkColNames: Set[String] = primaryKeyColumns.map(_.name).toSet
  val colNames: Set[String] = columns.map(_.name).toSet

  def toIndentedString: IndentedString =
    IndentedString.section(s"table($tableName):")(
      IndentedString.section("primary-key-columns:")(primaryKeyColumns.map(_.toSql)),
      IndentedString.section("columns:")(columns.map(_.toSql)),
    )

}
object TableState {

  def fromTable(repr: TableRepr[?]): Either[DeriveError, TableState] = {
    val ref = TableRef(repr.schemaName, repr.tableName)
    val cols: ArraySeq[Column] = repr.rowRepr.columns.columns

    if (cols.length != cols.distinctBy(_.name).length)
      DeriveError(ref, NonDistinctColumnNames).asLeft
    else
      TableState(
        ref,
        repr.pk.rowRepr.columns.columns,
        cols,
        repr.builtForeignKeys.map(ForeignKeyState.fromRepr),
        repr.builtIndices.map(IndexState.fromRepr),
      ).asRight
  }

  def unsafeFromTable(repr: TableRepr[?]): TableState = fromTable(repr) match
    case Right(value) => value
    case Left(error)  => throw error

}
