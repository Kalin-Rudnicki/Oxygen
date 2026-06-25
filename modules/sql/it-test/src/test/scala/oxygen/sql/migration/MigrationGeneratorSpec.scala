package oxygen.sql.migration

import oxygen.core.Version
import oxygen.predef.test.*
import oxygen.sql.migration.MigrationGenerator.GenerateResult
import oxygen.sql.migration.model.MigrationState
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
    )

}
