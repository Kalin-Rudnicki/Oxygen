package oxygen.sql.migration

import oxygen.core.Version
import oxygen.predef.json.*
import oxygen.predef.test.*
import oxygen.sql.migration.MigrationCodecs.given
import oxygen.sql.migration.model.TableState
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*

object VersionCodecsSpec extends OxygenSpecDefault {

  // Proves the macro-derived `TableRepr` can use a `Version`-typed primary key (the CP2 risk):
  // if `RowRepr[Version]` weren't summonable, this would not compile.
  @schemaName("oxygen_migration")
  @tableName("version_probe")
  final case class VersionProbeRow(
      @primaryKey version: Version,
      label: String,
  )
  object VersionProbeRow extends TableCompanion[VersionProbeRow, Version](TableRepr.derived[VersionProbeRow])

  override def testSpec: TestSpec =
    suite("VersionCodecsSpec")(
      test("Version JSON codec round-trips") {
        val v = Version("1.2.0")
        assertTrue(v.toJsonStringCompact.fromJsonString[Version] == v.asRight)
      },
      test("Version maps to a TEXT SQL column in a derived table") {
        val tableState = TableState.unsafeFromTable(VersionProbeRow.tableRepr)
        val versionCol = tableState.columns.find(_.name == "version")
        assertTrue(
          versionCol.map(_.columnType).contains(Column.Type.Text),
          tableState.primaryKeyColumns.map(_.name) == scala.collection.immutable.ArraySeq("version"),
        )
      },
    )

}
