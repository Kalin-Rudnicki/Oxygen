package oxygen.sql

import java.util.UUID
import oxygen.predef.test.*
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import oxygen.sql.query.{PostgresCRUDRepo, TableCompanion}
import oxygen.storage.CRUDRepo
import scala.annotation.experimental
import zio.*

@experimental
object TableCompanionQuerySpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  private final case class PostgresPersonRepo(db: Database) extends PostgresCRUDRepo.Unmapped[UUID, Person] {
    override protected val companion: TableCompanion[Person, UUID] = Person
  }

  override def testSpec: TestSpec =
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
      test("can insert (or do nothing)") {
        def gen2(gid: UUID): UIO[(Person, Person)] =
          Person.generate(gid)().map { p1 => (p1, p1.copy(age = p1.age + 1)) }

        for {
          db <- ZIO.service[Database]
          groupId <- Random.nextUUID
          (p1_1, p1_2) <- gen2(groupId)
          (p2_1, p2_2) <- gen2(groupId)
          (p3_1, p3_2) <- gen2(groupId)
          (p4_1, p4_2) <- gen2(groupId)
          (p5_1, p5_2) <- gen2(groupId)
          repo: CRUDRepo[UUID, Person] = PostgresPersonRepo(db)

          writeRes1 <- repo.insertOrDoNothing(p1_1)
          writeRes2 <- repo.insertOrDoNothing(p1_2)
          writeRes3 <- repo.insertOrDoNothingAllCounted(p1_2, p2_1, p2_2)
          writeRes4 <- repo.insertOrDoNothingAllCounted(p1_2, p2_2, p3_1, p4_1, p5_1, p3_2, p4_2, p5_2)
          readRes1 <- repo.getByKeyOrDie(p1_1.id)
          readRes2 <- repo.getByKeyOrDie(p2_1.id)
          readRes3 <- repo.getByKeyOrDie(p3_1.id)
          readRes4 <- repo.getByKeyOrDie(p4_1.id)
          readRes5 <- repo.getByKeyOrDie(p5_1.id)
        } yield assertTrue(
          writeRes1,
          !writeRes2,
          writeRes3 == (inserted = 1, ignored = 2),
          writeRes4 == (inserted = 3, ignored = 5),
          readRes1 == p1_1,
          readRes2 == p2_1,
          readRes3 == p3_1,
          readRes4 == p4_1,
          readRes5 == p5_1,
        )
      },
      test("works for arrays") {
        val v1 = Arrays(Nil, Set.empty, Nil, None)
        val v2 = Arrays(Nil, Set.empty, Nil, ArraySeq.empty[String].some)
        val v3 = Arrays(List(1, 2, 3), Set("D", "E", "F"), List(List(true, true, true), List(false, false, false), List(true, false, true)), ArraySeq("G", "H", "I").some)
        for {
          _ <- Arrays.insert.all(v1, v2, v3).unit
          res1 <- Arrays.selectAll.execute().to[Set]
        } yield assertTrue(res1 == Set(v1, v2, v3))
      },
    )

  override def testAspects: Chunk[TableCompanionQuerySpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic, TestAspect.withLiveClock)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      MigrationService.migrateLayer(
        Migrations(
          PlannedMigration.auto(1)(Person.tableRepr, Note.tableRepr, Ints.tableRepr, MultiPK1.tableRepr, MultiPK2.tableRepr, Arrays.tableRepr),
        ),
      ),
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

}
