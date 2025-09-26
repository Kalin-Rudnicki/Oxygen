package oxygen.sql

import oxygen.predef.test.*
import oxygen.sql.error.QueryError
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import oxygen.sql.query.*
import oxygen.sql.schema.*

object MigrationSpec extends OxygenSpec[Database & MigrationService] {

  @tableName("model_a")
  final case class ModelA1(
      @primaryKey id: Int,
      field1: String,
      field2: Boolean,
  )
  object ModelA1 extends TableCompanion[ModelA1, Int](TableRepr.derived[ModelA1])

  @tableName("model_a")
  final case class ModelA2(
      @primaryKey id: Int,
      field1: String,
      field2: Option[Boolean],
      other: Option[Int],
  )
  object ModelA2 extends TableCompanion[ModelA2, Int](TableRepr.derived[ModelA2])

  @tableName("model_a")
  final case class ModelA3(
      @primaryKey id: Int,
      field1: Option[Int], // different field type
      field2: Boolean,
  )
  object ModelA3 extends TableCompanion[ModelA3, Int](TableRepr.derived[ModelA3])

  @tableName("model_a")
  final case class ModelA4(
      id: Int, // missing pk
      field1: String,
      field2: Boolean,
  )
  object ModelA4 extends TableCompanion.NoKey[ModelA4](TableRepr.derived[ModelA4])

  @tableName("model_b")
  final case class ModelB1(
      @primaryKey id: Int,
      aId: Option[Int],
  )
  object ModelB1 extends TableCompanion[ModelB1, Int](TableRepr.derived[ModelB1])

  @tableName("model_b")
  final case class ModelB2(
      @primaryKey id: Int,
      @references[ModelA2] aId: Option[Int],
  )
  object ModelB2 extends TableCompanion[ModelB2, Int](TableRepr.derived[ModelB2])

  override def testSpec: TestSpec =
    suite("MigrationSpec")(
      test("simple migration works") {
        for {
          err1 <- ModelA1.selectAll.execute().arraySeq.exit
          migration1 = PlannedMigration.auto(1)(ModelA1.tableRepr, ModelB1.tableRepr)
          migration2 = PlannedMigration.auto(2)(ModelA2.tableRepr, ModelB2.tableRepr)
          migration3 = PlannedMigration.auto(3)(ModelA2.tableRepr, ModelB1.tableRepr)

          exe1 <- MigrationService.migrate(Migrations(migration1))
          err2 <- MigrationService.migrate(Migrations()).exit
          err3 <- MigrationService.migrate(Migrations(PlannedMigration.auto(1)(ModelA2.tableRepr, ModelB1.tableRepr))).exit
          exe2 <- MigrationService.migrate(Migrations(migration1))

          v1 = ModelA1(1, "value-1", true)
          v2 = ModelA1(2, "value-2", false)

          _ <- ModelA1.insert.all(v1, v2).unit
          get1 <- ModelA1.selectAll.execute().to[Seq]

          exe3 <- MigrationService.migrate(Migrations(migration1, migration2, migration3))
          get2 <- ModelA2.selectAll.execute().to[Seq]

          stage1 = Seq(v1, v2)
          stage2 = stage1.map { a1 => ModelA2(a1.id, a1.field1, a1.field2.some, None) }

        } yield assert(err1)(failsWithA[QueryError]) &&
          assert(err2)(failsWithA[MigrationError.MissingPlannedMigration]) &&
          assert(err3)(failsWithA[MigrationError.MigrationsDiffer]) &&
          assertTrue(
            exe1.executed.length == 1,
            exe2.executed.length == 0,
            exe3.executed.length == 2,
            get1.sortBy(_.id) == stage1,
            get2.sortBy(_.id) == stage2,
          )
      },
      test("diff validations") {
        for {
          err1 <- MigrationService.migrate(Migrations(PlannedMigration.auto(1)(ModelA1.tableRepr), PlannedMigration.auto(2)(ModelA3.tableRepr))).exit
          err2 <- MigrationService.migrate(Migrations(PlannedMigration.auto(1)(ModelA1.tableRepr), PlannedMigration.auto(2)(ModelA4.tableRepr))).exit
          _ <- MigrationService.migrate(
            Migrations(
              PlannedMigration.auto(1)(ModelA1.tableRepr),
              PlannedMigration.make(2)(ModelA3.tableRepr)(
                StateDiff.AlterColumn.DropColumn(EntityRef.ColumnRef("public", "model_a", "field_1")),
                PlannedMigration.StepType.auto,
              ),
            ),
          )
        } yield assert(err1)(failsWithA[MigrationError.ErrorDiffingState]) &&
          assert(err2)(failsWithA[MigrationError.ErrorDiffingState])
      },
    )

  // deliberately not shared.
  // this is expensive, but all these tests should really run against their own fresh database
  override def layerProvider: LayerProvider[R] =
    LayerProvider.providePerTest[Env](
      Helpers.databaseLayer,
      Helpers.testContainerLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

  override def defaultLogLevel: LogLevel = LogLevel.Trace

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.withLiveRandom, TestAspect.withLiveClock)

}
