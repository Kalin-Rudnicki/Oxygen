package oxygen.sql.migration

import java.util.UUID
import oxygen.predef.test.*
import oxygen.sql.migration.delta.MigrationPlanner
import oxygen.sql.migration.model.MigrationState
import oxygen.sql.migration.persistence.MigrationQueries
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*
import scala.collection.immutable.ArraySeq

object ExplicitNamingMigrationSpec extends OxygenSpecDefault {

  @tableName("mig_parent")
  final case class MigParent(@primaryKey id: UUID, name: String)
  object MigParent extends TableCompanion[MigParent, UUID](TableRepr.derived[MigParent])

  // explicit names on both the FK and the index
  @tableName("mig_child")
  @foreignKey.named[MigChild, MigParent]("fk_mig_child_parent", (_.parentId, _.id))
  @index.unique.named[MigChild]("idx_mig_child_name", _.name)
  final case class MigChild(@primaryKey id: UUID, parentId: UUID, name: String)
  object MigChild extends TableCompanion[MigChild, UUID](TableRepr.derived[MigChild])

  // auto-named counterpart, same shape
  @tableName("mig_child_auto")
  @foreignKey[MigChildAuto, MigParent]((_.parentId, _.id))
  @index.unique[MigChildAuto](_.name)
  final case class MigChildAuto(@primaryKey id: UUID, parentId: UUID, name: String)
  object MigChildAuto extends TableCompanion[MigChildAuto, UUID](TableRepr.derived[MigChildAuto])

  /** Genesis migration SQL (empty -> tables), joined for substring assertions. */
  private def genesisSql(reprs: TableRepr[?]*): String = {
    val state: MigrationState =
      MigrationState.fromTables(ArraySeq.from(reprs)) match
        case Right(s)    => s
        case Left(error) => throw new RuntimeException(s"could not derive state: $error")
    val diffs =
      MigrationPlanner.diffStates(MigrationState.empty, state) match
        case Right(d)    => d
        case Left(error) => throw new RuntimeException(s"could not diff states: $error")
    diffs.map(MigrationQueries.diffToQuery(_).ctx.sql).mkString("\n")
  }

  override def testSpec: TestSpec =
    suite("ExplicitNamingMigrationSpec")(
      test("explicit FK + index names are emitted verbatim in the migration DDL") {
        val sql = genesisSql(MigParent.tableRepr, MigChild.tableRepr)
        assertTrue(
          sql.contains("ADD CONSTRAINT fk_mig_child_parent"),
          sql.contains("CREATE UNIQUE INDEX idx_mig_child_name"),
        )
      },
      test("auto-named FK + index fall back to generated names (no explicit literal)") {
        val sql = genesisSql(MigParent.tableRepr, MigChildAuto.tableRepr)
        assertTrue(
          sql.contains("ADD CONSTRAINT fk____"),
          sql.contains("CREATE UNIQUE INDEX idx_u____"),
          !sql.contains("fk_mig_child_parent"),
          !sql.contains("idx_mig_child_name"),
        )
      },
    )

}
