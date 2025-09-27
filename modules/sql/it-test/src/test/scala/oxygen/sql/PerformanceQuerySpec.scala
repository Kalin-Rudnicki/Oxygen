package oxygen.sql

import oxygen.predef.test.{*, given}
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import scala.annotation.experimental
import zio.*

@experimental
object PerformanceQuerySpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  override def testSpec: TestSpec =
    suite("query performance")(
      test("selectByGroupId") {
        for {
          _ <- ZIO.logInfo("Running db performance spec")

          groupId <- Random.nextUUID

          insertGroupSize = 1000
          insertNumGroups = 10
          insertSize = insertGroupSize * insertNumGroups
          selectSize = 100

          getPar <- ZIO.parallelism
          _ <- ZIO.logDetailed(s"parallelize=$getPar")

          _ <- ZIO.logInfo("Generating data")
          generateInserts = Person.generate(groupId)().replicateZIO(insertGroupSize).replicateZIO(insertNumGroups).map { d => Chunk.from(d.map(Chunk.from)).zipWithIndexFrom(1) }
          people1 <- generateInserts
          people2 <- generateInserts
          people3 <- generateInserts
          people4 <- generateInserts
          people5 <- generateInserts.map(_.flatMap(_._1))

          hundred = Chunk.fill(selectSize)(())

          fetch = queries.selectByGroupId(groupId).to[Chunk]

          // =====| Inserts |=====

          _ <- ZIO.logInfo("Running non-parallel single inserts")
          (i1Duration, _) <-
            ZIO
              .foreachDiscard(people1) { case (chunk, i) =>
                ZIO.logDebug(s"non-parallel single insert ($i / $insertNumGroups)") *>
                  ZIO.foreachDiscard(chunk) { Person.insert(_).unit }
              }
              .timed
          _ <- ZIO.logInfo("Running parallel single inserts")
          (i2Duration, _) <-
            ZIO
              .foreachParDiscard(people2) { case (chunk, i) =>
                ZIO.logDebug(s"parallel single insert ($i / $insertNumGroups)") *>
                  ZIO.foreachParDiscard(chunk) { Person.insert(_).unit }
              }
              .timed
          _ <- ZIO.logInfo("Running non-parallel batch inserts")
          (i3Duration, _) <-
            ZIO
              .foreachDiscard(people3) { case (chunk, i) =>
                ZIO.logDebug(s"non-parallel batch insert ($i / $insertNumGroups)") *>
                  Person.insert.batched(chunk).unit
              }
              .timed
          _ <- ZIO.logInfo("Running parallel batch inserts")
          (i4Duration, _) <-
            ZIO
              .foreachParDiscard(people4) { case (chunk, i) =>
                ZIO.logDebug(s"non-parallel batch insert ($i / $insertNumGroups)") *>
                  Person.insert.batched(chunk).unit
              }
              .timed
          _ <- ZIO.logInfo("Running single batch insert")
          (i5Duration, _) <- Person.insert.batched(people5).unit.timed

          // =====| Selects |=====

          _ <- ZIO.logInfo("Running non-parallel selects")
          (s1Duration, _) <- ZIO.foreachDiscard(hundred) { _ => fetch }.timed
          _ <- ZIO.logInfo("Running parallel selects")
          (s2Duration, _) <- ZIO.foreachDiscard(hundred) { _ => fetch }.timed

          // =====| Data |=====

          durations = Chunk(
            i1Duration -> insertSize,
            i2Duration -> insertSize,
            i3Duration -> insertSize,
            i4Duration -> insertSize,
            i5Duration -> insertSize,
            s1Duration -> selectSize,
            s2Duration -> selectSize,
          )
          maxTotalLen = durations.map(_._1.render.length).max
          maxAvgLen = durations.map { case (d, c) => d.dividedBy(c).render.length }.max

          timings =
            s"""
               |=====| Timings |=====
               |non-parallel single inserts: ${i1Duration.render.alignRight(maxTotalLen)}, avg: ${i1Duration.dividedBy(insertSize).render.alignRight(maxAvgLen)}
               |parallel     single inserts: ${i2Duration.render.alignRight(maxTotalLen)}, avg: ${i2Duration.dividedBy(insertSize).render.alignRight(maxAvgLen)}
               |non-parallel  batch inserts: ${i3Duration.render.alignRight(maxTotalLen)}, avg: ${i3Duration.dividedBy(insertSize).render.alignRight(maxAvgLen)}
               |parallel      batch inserts: ${i4Duration.render.alignRight(maxTotalLen)}, avg: ${i4Duration.dividedBy(insertSize).render.alignRight(maxAvgLen)}
               |single        batch insert : ${i5Duration.render.alignRight(maxTotalLen)}, avg: ${i5Duration.dividedBy(insertSize).render.alignRight(maxAvgLen)}
               |
               |non-parallel        selects: ${s1Duration.render.alignRight(maxTotalLen)}, avg: ${s1Duration.dividedBy(selectSize).render.alignRight(maxAvgLen)}
               |    parallel        selects: ${s2Duration.render.alignRight(maxTotalLen)}, avg: ${s2Duration.dividedBy(selectSize).render.alignRight(maxAvgLen)}
               |""".stripMargin
          _ <- ZIO.logImportant(timings)
        } yield assertCompletes
      },
    ) @@ OxygenAspects.withMinLogLevel.detailed @@ TestAspect.tag("performance")

  override def testAspects: Chunk[PerformanceQuerySpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic, TestAspect.withLiveClock)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      MigrationService.migrateLayer(
        Migrations(
          PlannedMigration.auto(1)(Person.tableRepr, Note.tableRepr, Ints.tableRepr, MultiPK1.tableRepr, MultiPK2.tableRepr),
        ),
      ),
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

}
