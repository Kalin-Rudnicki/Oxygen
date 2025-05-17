package oxygen.sql

import java.util.UUID
import oxygen.predef.test.*
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.*
import oxygen.sql.query.{Helpers as QueryHelpers, *}
import oxygen.sql.query.dsl.*
import oxygen.sql.schema.*

object QuerySpec extends OxygenSpec[Database] {

  final case class Table1(
      @primaryKey id: UUID,
      externalId: UUID,
      str: String,
  )
  object Table1 {

    given tableRepr: TableRepr[Table1, UUID] = TableRepr.derived

    val insert: QueryI[Table1] = QueryHelpers.insertInto[Table1]

  }

  final case class Table2(
      @primaryKey id: UUID,
      table1Id: UUID,
      num: Int,
  )
  object Table2 {

    given tableRepr: TableRepr[Table2, UUID] = TableRepr.derived

    val insert: QueryI[Table2] = QueryHelpers.insertInto[Table2]

  }

  val get1: QueryIO[UUID, Table1] =
    for {
      i <- select.input[UUID]("get1")
      t1 <- from[Table1]
      _ <- where if t1.externalId == i
    } yield t1

  val get2: QueryIO[UUID, Table2] =
    for {
      i <- select.input[UUID]("get1")
      t1 <- from[Table1]
      t2 <- join[Table2] if t2.table1Id == t1.id
      _ <- where if t1.externalId == i
    } yield t2

  val get3: QueryIO[UUID, (Table1, Table2)] =
    for {
      i <- select.input[UUID]("get1")
      t1 <- from[Table1]
      t2 <- join[Table2] if t2.table1Id == t1.id
      _ <- where if t1.externalId == i
    } yield (t1, t2)

  val randomString: UIO[String] =
    Random.nextString(10)

  def randomT1(externalId: UUID): UIO[Table1] =
    for {
      id <- Random.nextUUID
      str <- randomString
    } yield Table1(id, externalId, str)

  def randomT2(t1: Table1): UIO[Table2] =
    for {
      id <- Random.nextUUID
      num <- Random.nextInt
    } yield Table2(id, t1.id, num)

  override def testSpec: TestSpec =
    suite("QuerySpec")(
      test("select + joins work") {
        for {
          externalId1 <- Random.nextUUID
          externalId2 <- Random.nextUUID
          externalId3 <- Random.nextUUID

          t1_1 <- randomT1(externalId1)
          t1_2 <- randomT1(externalId1)
          t1_3 <- randomT1(externalId1)
          t1_4 <- randomT1(externalId2)
          t1_5 <- randomT1(externalId2)

          t2_1_1 <- randomT2(t1_1)
          t2_1_2 <- randomT2(t1_1)
          t2_1_3 <- randomT2(t1_1)
          t2_2_1 <- randomT2(t1_2)
          t2_4_1 <- randomT2(t1_4)
          t2_4_2 <- randomT2(t1_4)

          _ <- Table1.insert.all(t1_1, t1_2, t1_3, t1_4, t1_5).unit
          _ <- Table2.insert.all(t2_1_1, t2_1_2, t2_1_3, t2_2_1, t2_4_1, t2_4_2).unit

          get1_1 <- get1.execute(externalId1).to[Seq]
          get1_2 <- get1.execute(externalId2).to[Seq]
          get1_3 <- get1.execute(externalId3).to[Seq]
          get2_1 <- get2.execute(externalId1).to[Seq]
          get2_2 <- get2.execute(externalId2).to[Seq]
          get2_3 <- get2.execute(externalId3).to[Seq]
          get3_1 <- get3.execute(externalId1).to[Seq]

        } yield assert(get1_1)(hasSameElements(Seq(t1_1, t1_2, t1_3))) &&
          assert(get1_2)(hasSameElements(Seq(t1_4, t1_5))) &&
          assert(get1_3)(isEmpty) &&
          assert(get2_1)(hasSameElements(Seq(t2_1_1, t2_1_2, t2_1_3, t2_2_1))) &&
          assert(get2_2)(hasSameElements(Seq(t2_4_1, t2_4_2))) &&
          assert(get2_3)(isEmpty) &&
          assert(get3_1)(hasSameElements(Seq(t1_1 -> t2_1_1, t1_1 -> t2_1_2, t1_1 -> t2_1_3, t1_2 -> t2_2_1)))
      },
    )

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[R](
      Helpers.databaseLayer,
      Helpers.testContainerLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
      MigrationService.migrateLayer(
        Migrations(
          PlannedMigration.auto(1)(
            Table1.tableRepr,
            Table2.tableRepr,
          ),
        ),
      ),
    )

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.withLiveRandom, TestAspect.withLiveClock)

}
