package oxygen.sql.migration

import oxygen.predef.json.*
import oxygen.predef.test.*
import oxygen.sql.migration.persistence.model.*
import scala.collection.immutable.ArraySeq

object PersistedMigrationFileSpec extends OxygenSpecDefault {

  private val userTableRef: EntityRefColumn.TableRef =
    EntityRefColumn.TableRef("public", "user")

  private val emailIndex: IndexColumn =
    IndexColumn(
      idxName = "idx_u____public__user____email",
      idxNameIsExplicit = false,
      self = userTableRef,
      unique = true,
      columns = ArraySeq("email"),
    )

  private val userTable: TableStateColumn =
    TableStateColumn(
      tableName = userTableRef,
      primaryKeyColumns = ArraySeq("id"),
      columns = ArraySeq(
        ColumnColumn("id", ColumnColumn.Type.UUID, nullable = false),
        ColumnColumn("email", ColumnColumn.Type.Text, nullable = false),
        ColumnColumn("display_name", ColumnColumn.Type.Text, nullable = true),
      ),
      foreignKeys = None,
      indices = Some(ArraySeq(emailIndex)),
    )

  private val exampleFile: PersistedMigrationFile =
    PersistedMigrationFile(
      formatVersion = PersistedMigrationFile.currentFormatVersion,
      version = "1.2.0",
      previousVersion = Some("1.1.0"),
      compatibility = MigrationCompatibility.BackwardsCompatible,
      diff = ArraySeq(
        PersistedMigrationFile.Step(
          stepNo = 0,
          derived = false,
          step = MigrationStepColumn.AlterColumn.CreateColumn(
            userTableRef,
            ColumnColumn("display_name", ColumnColumn.Type.Text, nullable = true),
          ),
        ),
        PersistedMigrationFile.Step(
          stepNo = 1,
          derived = false,
          step = MigrationStepColumn.AlterIndex.CreateIndex(emailIndex),
        ),
      ),
      state = MigrationStateColumn(
        extensions = Set.empty,
        schemas = Set("public"),
        tables = ArraySeq(userTable),
      ),
    )

  override def testSpec: TestSpec =
    suite("PersistedMigrationFileSpec")(
      test("round-trips through JSON") {
        assertTrue(exampleFile.toJsonStringCompact.fromJsonString[PersistedMigrationFile] == exampleFile.asRight)
      },
      test("diff steps are leaf-keyed (no nested sealed-trait wrappers)") {
        val json = exampleFile.toJsonStringCompact
        assertTrue(
          json.contains("\"CreateColumn\""),
          json.contains("\"CreateIndex\""),
          !json.contains("\"StateDiff\""),
          !json.contains("\"AlterColumn\""),
        )
      },
    )

}
