package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.schema.IndexRepr

final case class IndexState(
    explicitIdxName: Option[String],
    self: EntityRef.TableRef,
    unique: Boolean,
    columns: ArraySeq[String],
) derives Show {

  lazy val autoRef: IndexState.AutoRef =
    IndexState.AutoRef(self, unique, columns)

  val idxName: String = explicitIdxName.getOrElse(autoRef.autoIdxName)

  def ref: EntityRef.IndexRef = EntityRef.IndexRef(self, idxName)

  def renameSchema(schemaRef: SchemaRef, newName: String): IndexState = {
    extension (tr: TableRef) def updated: TableRef = if tr.schema == schemaRef then TableRef(newName, tr.tableName) else tr
    copy(self = self.updated)
  }

  def renameTable(tableRef: TableRef, newName: String): IndexState = {
    extension (tr: TableRef) def updated: TableRef = if tr == tableRef then TableRef(tr.schema, newName) else tr
    copy(self = self.updated)
  }

  def renameColumn(columnRef: ColumnRef, newName: String): IndexState =
    if this.self == columnRef.table then this.renameSelfColumn(columnRef.columnName, newName)
    else this

  def renameSelfColumn(oldName: String, newName: String): IndexState =
    copy(
      columns = this.columns.map {
        case `oldName` => newName
        case p         => p
      },
    )

}
object IndexState {

  final case class AutoRef(
      self: EntityRef.TableRef,
      unique: Boolean,
      columns: ArraySeq[String],
  ) {

    extension (repr: EntityRef.TableRef)
      private def fkScope: String =
        if repr.schema.isPublic then repr.tableName
        else s"${repr.schema.schemaName}__${repr.tableName}"

    private def prefix: String =
      if unique then "idx_u"
      else "idx"

    def autoIdxName: String =
      s"${prefix}____${self.fkScope}____${columns.mkString("__")}"

  }

  def fromRepr(repr: IndexRepr.Built): IndexState =
    IndexState(
      explicitIdxName = repr.explicitName,
      unique = repr.unique,
      self = EntityRef.TableRef(repr.self.schemaName, repr.self.tableName),
      columns = repr.columns.map(_.name),
    )

}
