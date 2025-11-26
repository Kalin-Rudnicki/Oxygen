package oxygen.sql

import oxygen.predef.test.*
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import oxygen.sql.test.SqlAspects
import scala.annotation.experimental
import zio.*

@experimental
object CustomQuerySpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  override def testSpec: TestSpec =
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
          p2 <- Person.generate(groupId)()

          _ <- Person.insert.all(p1, p2).unit

          oldGetBefore <- Person.selectByPK(p1.id).single
          oldSet <- queries.setAgeTo0_oldSyntax(p1).single
          oldGetAfter <- Person.selectByPK(p1.id).single

          newGetBefore <- Person.selectByPK(p2.id).single
          newSet <- queries.setAgeTo0_oldSyntax(p2).single
          newGetAfter <- Person.selectByPK(p2.id).single

        } yield assertTrue(
          oldGetBefore == p1,
          oldGetAfter == p1.copy(age = 0),
          oldSet == oldGetAfter,
          newGetBefore == p2,
          newGetAfter == p2.copy(age = 0),
          newSet == newGetAfter,
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
      test("orderBy / limit / offset") {
        val i11 = Ints(1, 1)
        val i12 = Ints(1, 2)
        val i13 = Ints(1, 3)
        val i21 = Ints(2, 1)
        val i22 = Ints(2, 2)
        val i23 = Ints(2, 3)
        val i31 = Ints(3, 1)
        val i32 = Ints(3, 2)
        val i33 = Ints(3, 3)

        for {
          _ <- Ints.insert.all(i11, i12, i13, i21, i22, i23, i31, i32, i33).unit

          res1 <- queries.intsConstLimit.execute().arraySeq
          res2 <- queries.intsDynamicLimit.execute(3).arraySeq
          res3 <- queries.intsDynamicLimit.execute(10).arraySeq

          res4 <- queries.intsOrderByA.execute(3).to[Set]
          res5 <- queries.intsOrderByAB.execute(5).to[List]
          res6 <- queries.intsOrderByBA.execute(5).to[List]

          res7 <- queries.intsOrderByABOffset.execute(5, 2).to[List]
          res8 <- queries.intsOrderByABOffset.execute(5, 6).to[List]

        } yield assertTrue(
          res1.length == 5,
          res2.length == 3,
          res3.length == 9,
          res4 == Set(i11, i12, i13),
          res5 == List(i13, i12, i11, i23, i22),
          res6 == List(i13, i23, i33, i12, i22),
          res7 == List(i11, i23, i22, i21, i33),
          res8 == List(i33, i32, i31),
        )
      },
      test("optional inputs") {
        for {
          groupId <- Random.nextUUID

          randomName = RandomGen.lowerCaseString(25)
          f1 <- randomName
          f2 <- randomName

          l1 <- randomName
          l2 <- randomName

          p1 <- Person.generate(groupId)(first = f1, last = l1)
          p2 <- Person.generate(groupId)(first = f1, last = l2)
          p3 <- Person.generate(groupId)(first = f2, last = l1)
          p4 <- Person.generate(groupId)(first = f2, last = l2)

          _ <- Person.insert.all(p1, p2, p3, p4).unit
          res1 <- queries.personSearch.execute(None, None).to[Set]
          res2 <- queries.personSearch.execute(f1.some, None).to[Set]
          res3 <- queries.personSearch.execute(None, l1.some).to[Set]
          res4 <- queries.personSearch.execute(f1.some, l1.some).to[Set]

          res5 <- queries.personSearchCountConst.execute(f1.some, None).single
          res6 <- queries.personSearchCountConst.execute(None, l1.some).single

        } yield assertTrue(
          res1 == Set(p1, p2, p3, p4),
          res2 == Set(p1, p2),
          res3 == Set(p1, p3),
          res4 == Set(p1),
          res5 == 2,
          res6 == 2,
        )
      },
      test("insert from select") {
        for {
          groupId <- Random.nextUUID

          randomName = RandomGen.lowerCaseString(25)
          f1 <- randomName
          f2 <- randomName

          l1 <- randomName
          l2 <- randomName

          p1 <- Person.generate(groupId)(first = f1, last = l1)
          p2 <- Person.generate(groupId)(first = f1, last = l2)
          p3 <- Person.generate(groupId)(first = f2, last = l1)
          p4 <- Person.generate(groupId)(first = f2, last = l2)

          _ <- Person.insert.all(p1, p2, p3, p4).unit
          _ <- queries.insertNoteForPeople.execute().unit
          _ <- queries.insertNoteForPeople2.execute(f1, "specific").unit

          res <- Note.selectAll.map(n => (n.personId, n.note)).execute().to[Set]
        } yield assertTrue(
          res == Set(
            (p1.id, s"Adding note for person ${p1.first} ${p1.last} : everyone"),
            (p2.id, s"Adding note for person ${p2.first} ${p2.last} : everyone"),
            (p3.id, s"Adding note for person ${p3.first} ${p3.last} : everyone"),
            (p4.id, s"Adding note for person ${p4.first} ${p4.last} : everyone"),
            (p1.id, s"Adding note for person ${p1.first} ${p1.last} : specific"),
            (p2.id, s"Adding note for person ${p2.first} ${p2.last} : specific"),
          ),
        )
      },
    )

  override def testAspects: Chunk[CustomQuerySpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic, TestAspect.withLiveClock, SqlAspects.isolateTestsInRollbackTransaction)

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
