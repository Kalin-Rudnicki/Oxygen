package oxygen.sql

import java.util.UUID
import oxygen.predef.test.{*, given}
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import scala.annotation.experimental
import zio.*

@experimental
object QuerySpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  extension [A](self: Specified[A])
    private def orGen(eff: => UIO[A]): UIO[A] = self match
      case Specified.WasSpecified(value) => ZIO.succeed(value)
      case Specified.WasNotSpecified     => eff

  private val randomName: UIO[String] =
    Random.nextIntBetween('a'.toInt, 'z'.toInt + 1).map(_.toChar).replicateZIO(10).map(_.mkString)

  private def randomPerson(groupId: UUID)(
      id: Specified[UUID] = Specified.WasNotSpecified,
      first: Specified[String] = Specified.WasNotSpecified,
      last: Specified[String] = Specified.WasNotSpecified,
      age: Specified[Int] = Specified.WasNotSpecified,
  ): UIO[Person] =
    for {
      id <- id.orGen { Random.nextUUID }
      first <- first.orGen { randomName }
      last <- last.orGen { randomName }
      age <- age.orGen { Random.nextIntBetween(0, 100) }
    } yield Person(id, groupId, first, last, age)

  private def tableCompanionSpec: TestSpec =
    suite("TableCompanion queries")(
      test("insert & select") {
        for {
          groupId <- Random.nextUUID
          p1 <- randomPerson(groupId)()
          p2 <- randomPerson(groupId)()
          p1p2 = Set(p1, p2)
          otherId <- Random.nextUUID

          all1 <- Person.selectAll().contiguous
          _ <- Person.insert.all(p1, p2).unit
          all2 <- Person.selectAll().contiguous

          getP1Opt <- Person.selectByPK(p1.id).option
          getP1Req <- Person.selectByPK(p1.id).single
          getP2Opt <- Person.selectByPK(p2.id).option
          getP2Req <- Person.selectByPK(p2.id).single
          getOtherOpt <- Person.selectByPK(otherId).option
          getOtherReq <- Person.selectByPK(otherId).single.exit
        } yield assertTrue(
          (all1.toSet & p1p2).isEmpty,
          (all2.toSet & p1p2) == p1p2,
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
          p1 <- randomPerson(groupId)()
          p2 <- randomPerson(groupId)()

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
          p1 <- randomPerson(groupId)()
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
    )

  private def customQuerySpec: TestSpec =
    suite("custom queries")(
      test("selectByGroupId") {
        for {
          groupId1 <- Random.nextUUID
          groupId2 <- Random.nextUUID
          p1s <- randomPerson(groupId1)().replicateZIO(10)
          p2s <- randomPerson(groupId2)().replicateZIO(10)

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
          p1 <- randomPerson(groupId)()

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
          p1s <- randomPerson(groupId1)().replicateZIO(10)
          p2s <- randomPerson(groupId2)().replicateZIO(10)

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
          p1 <- randomPerson(groupId)()
          othersSame <- randomPerson(groupId)(last = p1.last).replicateZIO(10).map(_.toSet)
          othersNotSame <- randomPerson(groupId)().replicateZIO(10).map(_.toSet)

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
    )

  override def testSpec: TestSpec =
    suite("QuerySpec")(
      tableCompanionSpec,
      customQuerySpec,
    )

  override def testAspects: Chunk[QuerySpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[R](
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
