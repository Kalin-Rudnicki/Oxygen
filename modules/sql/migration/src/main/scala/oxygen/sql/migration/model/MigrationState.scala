package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiffError.*
import oxygen.sql.schema.TableRepr

final case class MigrationState(
    extensions: Set[String],
    schemas: Set[SchemaRef],
    tables: Map[TableRef, TableState],
) {

  def toIndentedString: IndentedString =
    IndentedString.section("Migration State:")(
      IndentedString.section("extensions:")(extensions.toSeq.sorted),
      IndentedString.section("schemas:")(schemas.toSeq.sorted.map(_.toString)),
      IndentedString.section("tables:")(tables.values.toSeq.sortBy(_.tableName).map(_.toIndentedString)),
    )

}
object MigrationState {

  val empty: MigrationState = MigrationState(Set.empty, Set(SchemaRef("public")), Map.empty)

  def fromTables(schemas: ArraySeq[TableRepr[?]]): Either[DeriveError, MigrationState] =
    schemas.traverse(TableState.fromTable).map { tables =>
      MigrationState(
        tables.flatMap(_.columns).flatMap(_.columnType.extension).toSet,
        tables.map(_.tableName.schema).toSet + SchemaRef("public"),
        tables.map(t => (t.tableName, t)).toMap,
      )
    }

}
