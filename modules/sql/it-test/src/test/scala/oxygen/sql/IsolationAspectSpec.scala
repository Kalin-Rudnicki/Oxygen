package oxygen.sql

import oxygen.predef.test.*
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import oxygen.sql.test.SqlAspects
import scala.annotation.experimental
import zio.*

@experimental
object IsolationAspectSpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  def makeTest(n: Int, isolated: Boolean): TestSpec =
    test(s"test - $n (${if isolated then "" else "NOT "}isolated)") {
      for {
        groupId <- Random.nextUUID
        people <- Person.generate(groupId)().replicateZIO(10).map(_.toSet)

        _ <- Person.insert.batched(people).unit
        _ <- Clock.sleep(100.millis)

        selectByGroupId <- queries.selectByGroupId(groupId).to[Set]
        selectAll <- Person.selectAll().to[Set]
      } yield assertTrue(
        selectByGroupId == people,
        if isolated then selectAll == people
        else selectAll != people && selectAll.size > people.size,
      )
    }

  def tableIsEmptySpec(shouldBeEmpty: Boolean): TestSpec =
    test(s"test whether person table IS${if shouldBeEmpty then "" else " NOT"} empty") {
      for {
        selectAll <- Person.selectAll().to[Set]
      } yield assertTrue(
        if shouldBeEmpty then selectAll.isEmpty
        else selectAll.nonEmpty,
      )
    }

  def makeSpec(isolated: Boolean): TestSpec =
    suite(s"${if isolated then "" else "NOT "}isolated")(
      makeTest(1, isolated),
      makeTest(2, isolated),
      makeTest(3, isolated),
      makeTest(4, isolated),
      makeTest(5, isolated),
    ) @@ TestAspect.parallel

  override def testSpec: TestSpec =
    suite("IsolationAspectSpec")(
      makeSpec(true) @@ SqlAspects.isolateTestsInRollbackTransaction,
      tableIsEmptySpec(true),
      makeSpec(false),
      tableIsEmptySpec(false),
    ) @@ TestAspect.sequential

  override def testAspects: Chunk[IsolationAspectSpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic, TestAspect.withLiveClock)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      MigrationService.migrateLayer(
        Migrations(
          PlannedMigration.auto(1)(Person.tableRepr),
        ),
      ),
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

}
