package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.schema.ForeignKeyRepr

final case class ForeignKeyState(
    explicitFKName: Option[String],
    self: EntityRef.TableRef,
    references: EntityRef.TableRef,
    columnPairs: ArraySeq[ForeignKeyState.Pair],
) derives Show {

  lazy val autoRef: ForeignKeyState.AutoRef =
    ForeignKeyState.AutoRef(self, references, columnPairs)

  val fkName: String = explicitFKName.getOrElse(autoRef.autoFKName)

  def ref: EntityRef.ForeignKeyRef = EntityRef.ForeignKeyRef(self, fkName)

  def renameSchema(schemaRef: SchemaRef, newName: String): ForeignKeyState = {
    extension (tr: TableRef) def updated: TableRef = if tr.schema == schemaRef then TableRef(newName, tr.tableName) else tr
    copy(self = self.updated, references = references.updated)
  }

  def renameTable(tableRef: TableRef, newName: String): ForeignKeyState = {
    extension (tr: TableRef) def updated: TableRef = if tr == tableRef then TableRef(tr.schema, newName) else tr
    copy(self = self.updated, references = references.updated)
  }

  def renameColumn(columnRef: ColumnRef, newName: String): ForeignKeyState =
    (this.self == columnRef.table, this.references == columnRef.table) match
      case (true, true)   => this.renameSelfColumn(columnRef.columnName, newName).renameReferencesColumn(columnRef.columnName, newName)
      case (true, false)  => this.renameSelfColumn(columnRef.columnName, newName)
      case (false, true)  => this.renameReferencesColumn(columnRef.columnName, newName)
      case (false, false) => this

  def renameSelfColumn(oldName: String, newName: String): ForeignKeyState =
    copy(
      columnPairs = this.columnPairs.map {
        case ForeignKeyState.Pair(`oldName`, references) => ForeignKeyState.Pair(newName, references)
        case p                                           => p
      },
    )

  def renameReferencesColumn(oldName: String, newName: String): ForeignKeyState =
    copy(
      columnPairs = this.columnPairs.map {
        case ForeignKeyState.Pair(self, `oldName`) => ForeignKeyState.Pair(self, newName)
        case p                                     => p
      },
    )

}
object ForeignKeyState {

  final case class AutoRef(
      self: EntityRef.TableRef,
      references: EntityRef.TableRef,
      columnPairs: ArraySeq[ForeignKeyState.Pair],
  ) {

    extension (repr: EntityRef.TableRef)
      private def fkScope: String =
        if repr.schema.isPublic then repr.tableName
        else s"${repr.schema.schemaName}__${repr.tableName}"

    def autoFKName: String =
      s"fk____${self.fkScope}___${references.fkScope}____${columnPairs.map(p => s"${p.self}__${p.references}").mkString("___")}"

  }

  final case class Pair(
      self: String,
      references: String,
  ) derives Show

  def fromRepr(repr: ForeignKeyRepr.Built): ForeignKeyState =
    ForeignKeyState(
      explicitFKName = repr.explicitName,
      self = EntityRef.TableRef(repr.self.schemaName, repr.self.tableName),
      references = EntityRef.TableRef(repr.references.schemaName, repr.references.tableName),
      columnPairs = repr.columnPairs.map { p => Pair(p.self.name, p.references.name) },
    )

}
