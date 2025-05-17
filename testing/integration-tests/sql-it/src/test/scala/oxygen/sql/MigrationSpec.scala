package oxygen.sql

import oxygen.predef.test.*
import oxygen.sql.error.QueryError
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import oxygen.sql.query.{Helpers as QueryHelpers, *}
import oxygen.sql.query.dsl.*
import oxygen.sql.schema.*

object MigrationSpec extends OxygenSpec[Database & MigrationService] {

  @tableName("model_a")
  final case class ModelA1(
      @primaryKey id: Int,
      field1: String,
      field2: Boolean,
  )
  object ModelA1 {

    given tableRepr: TableRepr[ModelA1, Int] = TableRepr.derived

    val insert: QueryI[ModelA1] = QueryHelpers.insertInto[ModelA1]

    val getAll: QueryO[ModelA1] =
      for {
        _ <- select("select a1")
        a1 <- from[ModelA1]
      } yield a1

  }

  @tableName("model_a")
  final case class ModelA2(
      @primaryKey id: Int,
      field1: String,
      field2: Option[Boolean],
      other: Option[Int],
  )
  object ModelA2 {

    given tableRepr: TableRepr[ModelA2, Int] = TableRepr.derived

    val getAll: QueryO[ModelA2] =
      for {
        _ <- select("select a2")
        a2 <- from[ModelA2]
      } yield a2

  }

  @tableName("model_a")
  final case class ModelA3(
      @primaryKey id: Int,
      field1: Option[Int], // different field type
      field2: Boolean,
  )
  object ModelA3 {
    given tableRepr: TableRepr[ModelA3, Int] = TableRepr.derived
  }

  @tableName("model_a")
  final case class ModelA4(
      id: Int, // missing pk
      field1: String,
      field2: Boolean,
  )
  object ModelA4 {
    given tableRepr: TableRepr[ModelA4, Unit] = TableRepr.derived
  }

  override def testSpec: TestSpec =
    suite("MigrationSpec")(
      test("simple migration works") {
        for {
          err1 <- ModelA1.getAll.execute().contiguous.exit
          migration1 = PlannedMigration.auto(1)(ModelA1.tableRepr)
          migration2 = PlannedMigration.auto(2)(ModelA2.tableRepr)

          exe1 <- MigrationService.migrate(Migrations(migration1))
          err2 <- MigrationService.migrate(Migrations()).exit
          err3 <- MigrationService.migrate(Migrations(PlannedMigration.auto(1)(ModelA2.tableRepr))).exit
          exe2 <- MigrationService.migrate(Migrations(migration1))

          v1 = ModelA1(1, "value-1", true)
          v2 = ModelA1(2, "value-2", false)

          _ <- ModelA1.insert.all(v1, v2).unit
          get1 <- ModelA1.getAll.execute().to[Seq]

          exe3 <- MigrationService.migrate(Migrations(migration1, migration2))
          get2 <- ModelA2.getAll.execute().to[Seq]

          stage1 = Seq(v1, v2)
          stage2 = stage1.map { a1 => ModelA2(a1.id, a1.field1, a1.field2.some, None) }

        } yield assert(err1)(failsWithA[QueryError]) &&
          assert(err2)(failsWithA[MigrationError.MissingPlannedMigration]) &&
          assert(err3)(failsWithA[MigrationError.MigrationsDiffer]) &&
          assertTrue(
            exe1.executed.length == 1,
            exe2.executed.length == 0,
            exe3.executed.length == 1,
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
    LayerProvider.providePerTest[R](
      Helpers.databaseLayer,
      Helpers.testContainerLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

  override def defaultLogLevel: LogLevel = LogLevel.Info

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.withLiveRandom, TestAspect.withLiveClock)

}
