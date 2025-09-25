package oxygen.sql

import oxygen.predef.test.{*, given}
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import scala.annotation.experimental
import zio.*

@experimental
object QuerySpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  private def tableCompanionSpec: TestSpec =
    suite("TableCompanion queries")(
      test("insert & select") {
        for {
          groupId <- Random.nextUUID
          p1 <- Person.generate(groupId)()
          p2 <- Person.generate(groupId)()
          p1p2 = Set(p1, p2)
          otherId <- Random.nextUUID

          all1 <- Person.selectAll().arraySeq
          _ <- Person.insert.all(p1, p2).unit
          all2 <- Person.selectAll().to[Set]

          getP1Opt <- Person.selectByPK(p1.id).option
          getP1Req <- Person.selectByPK(p1.id).single
          getP2Opt <- Person.selectByPK(p2.id).option
          getP2Req <- Person.selectByPK(p2.id).single
          getOtherOpt <- Person.selectByPK(otherId).option
          getOtherReq <- Person.selectByPK(otherId).single.exit
        } yield assertTrue(
          (all1.toSet & p1p2).isEmpty,
          (all2 & p1p2) == p1p2,
          getP1Opt.contains(p1),
          getP1Req == p1,
          getP2Opt.contains(p2),
          getP2Req == p2,
          getOtherOpt.isEmpty,
          getOtherReq.isFailure,
        )
      },
      test("can delete") {
        for {
          groupId <- Random.nextUUID
          p1 <- Person.generate(groupId)()
          p2 <- Person.generate(groupId)()

          _ <- Person.insert(p2).unit

          p1Get1 <- Person.selectByPK(p1.id).option
          _ <- Person.insert(p1).unit
          p1Get2 <- Person.selectByPK(p1.id).option
          _ <- Person.deleteByPK(p1.id).unit
          p1Get3 <- Person.selectByPK(p1.id).option

          p2Get <- Person.selectByPK(p2.id).option
        } yield assertTrue(
          p1Get1.isEmpty,
          p1Get2.contains(p1),
          p1Get3.isEmpty,
          p2Get.contains(p2),
        )
      },
      test("can update") {
        for {
          groupId <- Random.nextUUID
          p1 <- Person.generate(groupId)()
          updatedP1 = p1.copy(age = p1.age + 1)

          _ <- Person.insert(p1).unit
          p1Get1 <- Person.selectByPK(p1.id).single
          _ <- Person.update(updatedP1).unit
          p1Get2 <- Person.selectByPK(p1.id).single
        } yield assertTrue(
          p1Get1 == p1,
          p1Get2 == updatedP1,
        )
      },
      test("can upsert") {
        for {
          groupId <- Random.nextUUID
          p1 <- Person.generate(groupId)()
          p2 = p1.copy(age = p1.age + 1)

          _ <- Person.insert.execute(p1).unit
          err1 <- Person.insert.execute(p2).unit.exit
          _ <- Person.upsert.execute(p2).unit
          res1 <- Person.selectByPK.execute(p1.id).single
        } yield assert(err1)(fails(anything)) && assertTrue(res1 == p2)
      },
    )

  private def customQuerySpec: TestSpec =
    suite("custom queries")(
      test("selectByGroupId") {
        for {
          groupId1 <- Random.nextUUID
          groupId2 <- Random.nextUUID
          p1s <- Person.generate(groupId1)().replicateZIO(10)
          p2s <- Person.generate(groupId2)().replicateZIO(10)

          _ <- Person.insert.batched(p1s ++ p2s).unit
          getP1s <- queries.selectByGroupId(groupId1).to[Set]
          getP2s <- queries.selectByGroupId(groupId2).to[Set]
        } yield assertTrue(
          getP1s == p1s.toSet,
          getP2s == p2s.toSet,
        )
      },
      test("setAgeTo0") {
        for {
          groupId <- Random.nextUUID
          p1 <- Person.generate(groupId)()

          _ <- Person.insert(p1).unit
          get1 <- Person.selectByPK(p1.id).single
          set1 <- queries.setAgeTo0(p1).single
          get2 <- Person.selectByPK(p1.id).single
        } yield assertTrue(
          get1 == p1,
          get2 == p1.copy(age = 0),
          set1 == get2,
        )
      },
      test("deleteByGroupId") {
        for {
          groupId1 <- Random.nextUUID
          groupId2 <- Random.nextUUID
          p1s <- Person.generate(groupId1)().replicateZIO(10)
          p2s <- Person.generate(groupId2)().replicateZIO(10)

          _ <- Person.insert.batched(p1s ++ p2s).unit
          getP1s1 <- queries.selectByGroupId(groupId1).to[Set]
          getP2s1 <- queries.selectByGroupId(groupId2).to[Set]

          deleteP1s <- queries.deleteByGroupId(groupId1).to[Set]

          getP1s2 <- queries.selectByGroupId(groupId1).to[Set]
          getP2s2 <- queries.selectByGroupId(groupId2).to[Set]
        } yield assertTrue(
          getP1s1 == p1s.toSet,
          getP2s1 == p2s.toSet,
          getP1s2.isEmpty,
          getP1s1 == deleteP1s,
          getP2s2 == p2s.toSet,
        )
      },
      test("othersWithSameLastNameAsId") {
        for {
          groupId <- Random.nextUUID
          p1 <- Person.generate(groupId)()
          othersSame <- Person.generate(groupId)(last = p1.last).replicateZIO(10).map(_.toSet)
          othersNotSame <- Person.generate(groupId)().replicateZIO(10).map(_.toSet)

          run = queries.othersWithSameLastNameAsId(p1.id).to[Set]

          res1 <- run
          _ <- Person.insert(p1).unit
          res2 <- run
          _ <- Person.insert.batched(othersSame ++ othersNotSame).unit
          res3 <- run

        } yield assertTrue(
          res1.isEmpty,
          res2.isEmpty,
          res3 == othersSame,
        )
      },
      test("join vs left join") {
        for {
          groupId <- Random.nextUUID
          p1 <- Person.generate(groupId)()
          p2 <- Person.generate(groupId)()
          p3 <- Person.generate(groupId)()

          n2_1 <- Note.generate(p2.id)()
          n3_1 <- Note.generate(p3.id)()
          n3_2 <- Note.generate(p3.id)()
          n3_3 <- Note.generate(p3.id)()

          _ <- Person.insert.all(p1, p2, p3).unit
          _ <- Note.insert.all(n2_1, n3_1, n3_2, n3_3).unit

          leftJoinRes <- queries.personLeftJoinNotes.execute(groupId).to[Set]
          joinRes <- queries.personJoinNotes.execute(groupId).to[Set]

        } yield assertTrue(
          joinRes == Set(
            (p2, n2_1),
            (p3, n3_1),
            (p3, n3_2),
            (p3, n3_3),
          ),
          leftJoinRes == Set(
            (p1, None),
            (p2, n2_1.some),
            (p3, n3_1.some),
            (p3, n3_2.some),
            (p3, n3_3.some),
          ),
        )
      },
    )

  private def perfSpec: TestSpec =
    suite("performance")(
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

  override def testSpec: TestSpec =
    suite("QuerySpec")(
      tableCompanionSpec,
      customQuerySpec,
      perfSpec,
    )

  override def testAspects: Chunk[QuerySpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic, TestAspect.withLiveClock)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      MigrationService.migrateLayer(
        Migrations(
          PlannedMigration.auto(1)(Person.tableRepr, Note.tableRepr),
        ),
      ),
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

}
