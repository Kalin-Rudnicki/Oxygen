package oxygen.sql.migration

import oxygen.core.Version
import oxygen.predef.test.*
import oxygen.sql.migration.MigrationGenerator.GenerateResult
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.model.{MigrationCompatibility, MigrationStepColumn}
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*
import scala.collection.immutable.ArraySeq

object MigrationGeneratorSpec extends OxygenSpecDefault {

  @tableName("person")
  final case class PersonV1(@primaryKey id: Int, name: String)
  object PersonV1 extends TableCompanion[PersonV1, Int](TableRepr.derived[PersonV1])

  // adds a nullable column -> backwards-compatible (minor)
  @tableName("person")
  final case class PersonV2(@primaryKey id: Int, name: String, nickname: Option[String])
  object PersonV2 extends TableCompanion[PersonV2, Int](TableRepr.derived[PersonV2])

  // adds a non-nullable column -> incompatible (major)
  @tableName("person")
  final case class PersonV3(@primaryKey id: Int, name: String, age: Int)
  object PersonV3 extends TableCompanion[PersonV3, Int](TableRepr.derived[PersonV3])

  private def stateOf(reprs: TableRepr[?]*): MigrationState =
    MigrationState.fromTables(ArraySeq.from(reprs)) match
      case Right(s)    => s
      case Left(error) => throw new RuntimeException(s"could not derive state: $error")

  private def generated(result: Either[Any, GenerateResult]): GenerateResult.Generated =
    result match
      case Right(g: GenerateResult.Generated) => g
      case other                              => throw new RuntimeException(s"expected Generated, got: $other")

  // Fixtures for exercising the classification table directly (pure -- no DB).
  private val personTable: TableState = TableState.unsafeFromTable(PersonV1.tableRepr)
  private val personRef: EntityRef.TableRef = personTable.tableName
  private val otherRef: EntityRef.TableRef = EntityRef.TableRef("public", "other")

  private def mkCol(name: String, nullable: Boolean): Column = Column(name, Column.Type.Int, nullable)
  private def colRef(name: String): EntityRef.ColumnRef = EntityRef.ColumnRef(personRef, name)
  private def fkOn(self: EntityRef.TableRef): ForeignKeyState = ForeignKeyState(None, self, otherRef, ArraySeq(ForeignKeyState.Pair("other_id", "id")))
  private def idxOn(self: EntityRef.TableRef, unique: Boolean): IndexState = IndexState(None, self, unique, ArraySeq("name"))

  private def classify(diffs: StateDiff*): MigrationCompatibility = MigrationGenerator.classify(ArraySeq.from(diffs))

  override def testSpec: TestSpec =
    suite("MigrationGeneratorSpec")(
      test("genesis migration -> 1.0.0, backwards-compatible") {
        val g = generated(MigrationGenerator.generate(None, stateOf(PersonV1.tableRepr)))
        assertTrue(
          g.file.version == "1.0.0",
          g.file.previousVersion.isEmpty,
          g.compatibility == MigrationCompatibility.BackwardsCompatible,
          g.file.state.tables.exists(_.tableName.table == "person"),
          g.file.diff.nonEmpty,
        )
      },
      test("no changes -> UpToDate") {
        val state = stateOf(PersonV1.tableRepr)
        assertTrue(
          MigrationGenerator.generate(Some((Version("1.0.0"), state)), state) == Right(GenerateResult.UpToDate),
        )
      },
      test("adding a nullable column -> minor bump, backwards-compatible") {
        val g = generated(MigrationGenerator.generate(Some((Version("1.0.0"), stateOf(PersonV1.tableRepr))), stateOf(PersonV2.tableRepr)))
        assertTrue(
          g.compatibility == MigrationCompatibility.BackwardsCompatible,
          g.file.version == "1.1.0",
          g.file.previousVersion.contains("1.0.0"),
          g.file.diff.exists(_.step.isInstanceOf[MigrationStepColumn.AlterColumn.CreateColumn]),
        )
      },
      test("adding a non-nullable column -> major bump, incompatible") {
        val g = generated(MigrationGenerator.generate(Some((Version("1.0.0"), stateOf(PersonV1.tableRepr))), stateOf(PersonV3.tableRepr)))
        assertTrue(
          g.compatibility == MigrationCompatibility.Incompatible,
          g.file.version == "2.0.0",
          g.file.previousVersion.contains("1.0.0"),
        )
      },
      suite("classify (classification table)")(
        test("empty diff -> BackwardsCompatible") {
          assertTrue(classify() == MigrationCompatibility.BackwardsCompatible)
        },
        test("CreateColumn: nullable is compatible, NOT NULL is incompatible") {
          assertTrue(
            classify(StateDiff.AlterColumn.CreateColumn(personRef, mkCol("nickname", nullable = true))) == MigrationCompatibility.BackwardsCompatible,
            classify(StateDiff.AlterColumn.CreateColumn(personRef, mkCol("age", nullable = false))) == MigrationCompatibility.Incompatible,
          )
        },
        test("SetNullable: relaxing is compatible, tightening is incompatible") {
          assertTrue(
            classify(StateDiff.AlterColumn.SetNullable(colRef("name"), nullable = true)) == MigrationCompatibility.BackwardsCompatible,
            classify(StateDiff.AlterColumn.SetNullable(colRef("name"), nullable = false)) == MigrationCompatibility.Incompatible,
          )
        },
        test("drops: column/table/schema are incompatible; foreign-key/index are compatible") {
          assertTrue(
            classify(StateDiff.AlterColumn.DropColumn(colRef("name"))) == MigrationCompatibility.Incompatible,
            classify(StateDiff.AlterTable.DropTable(personRef)) == MigrationCompatibility.Incompatible,
            classify(StateDiff.AlterSchema.DropSchema(EntityRef.SchemaRef("public"))) == MigrationCompatibility.Incompatible,
            classify(StateDiff.AlterForeignKey.DropForeignKey(fkOn(personRef).ref)) == MigrationCompatibility.BackwardsCompatible,
            classify(StateDiff.AlterIndex.DropIndex(idxOn(personRef, unique = true).ref)) == MigrationCompatibility.BackwardsCompatible,
          )
        },
        test("RenameColumn is incompatible") {
          assertTrue(classify(StateDiff.AlterColumn.RenameColumn(colRef("name"), "full_name")) == MigrationCompatibility.Incompatible)
        },
        test("foreign key: incompatible on an existing table, compatible on a same-migration new table") {
          assertTrue(
            classify(StateDiff.AlterForeignKey.CreateForeignKey(fkOn(personRef))) == MigrationCompatibility.Incompatible,
            classify(StateDiff.AlterTable.CreateTable(personTable), StateDiff.AlterForeignKey.CreateForeignKey(fkOn(personRef))) == MigrationCompatibility.BackwardsCompatible,
          )
        },
        test("index: unique incompatible on existing table, compatible on new table; non-unique always compatible") {
          assertTrue(
            classify(StateDiff.AlterIndex.CreateIndex(idxOn(personRef, unique = true))) == MigrationCompatibility.Incompatible,
            classify(StateDiff.AlterTable.CreateTable(personTable), StateDiff.AlterIndex.CreateIndex(idxOn(personRef, unique = true))) == MigrationCompatibility.BackwardsCompatible,
            classify(StateDiff.AlterIndex.CreateIndex(idxOn(personRef, unique = false))) == MigrationCompatibility.BackwardsCompatible,
          )
        },
        test("aggregate: any single incompatible diff makes the whole migration incompatible") {
          assertTrue(
            classify(
              StateDiff.AlterColumn.CreateColumn(personRef, mkCol("nickname", nullable = true)),
              StateDiff.AlterColumn.DropColumn(colRef("name")),
            ) == MigrationCompatibility.Incompatible,
            classify(
              StateDiff.AlterColumn.CreateColumn(personRef, mkCol("nickname", nullable = true)),
              StateDiff.AlterIndex.CreateIndex(idxOn(personRef, unique = false)),
            ) == MigrationCompatibility.BackwardsCompatible,
          )
        },
      ),
    )

}
