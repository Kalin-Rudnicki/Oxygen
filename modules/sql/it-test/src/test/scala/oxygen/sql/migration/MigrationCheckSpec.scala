package oxygen.sql.migration

import java.nio.file.Files
import oxygen.predef.test.*
import oxygen.sql.migration.MigrationCheck.{Config, Outcome}
import oxygen.sql.migration.model.MigrationSchema
import oxygen.sql.migration.persistence.MigrationFs
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*
import oxygen.zio.system.Path
import scala.collection.immutable.ArraySeq
import zio.*

object MigrationCheckSpec extends OxygenSpecDefault {

  @tableName("widget")
  final case class WidgetV1(@primaryKey id: Int, name: String)
  object WidgetV1 extends TableCompanion[WidgetV1, Int](TableRepr.derived[WidgetV1])

  // adds a non-nullable column -> incompatible
  @tableName("widget")
  final case class WidgetV2(@primaryKey id: Int, name: String, count: Int)
  object WidgetV2 extends TableCompanion[WidgetV2, Int](TableRepr.derived[WidgetV2])

  private val v1: MigrationSchema = MigrationSchema.of(WidgetV1.tableRepr)
  private val v2: MigrationSchema = MigrationSchema.of(WidgetV2.tableRepr)

  private val updateAllowed: Config = Config(allowUpdate = true, allowIncompatible = false)
  private val incompatibleAllowed: Config = Config(allowUpdate = true, allowIncompatible = true)

  extension [R, E, A](z: ZIO[R, E, A])
    private def orFail: ZIO[R, Nothing, A] = z.orDieWith(e => new RuntimeException(e.toString))

  private val tempDir: ZIO[Any, Nothing, Path] =
    for {
      tmp <- ZIO.attempt(Files.createTempDirectory("oxygen-migration-check")).orDie
      dir <- Path.of(tmp.toString).orDie
    } yield dir

  private def versionsOnDisk(dir: Path): ZIO[Any, Nothing, Seq[String]] =
    MigrationFs(dir).list.map(_.map(_.version.show)).orFail

  override def testSpec: TestSpec =
    suite("MigrationCheckSpec")(
      test("CI (no update) on a stale repo -> PendingUpdate and writes nothing") {
        for {
          dir <- tempDir
          outcome <- MigrationCheck.check(dir, v1, Config.ci).orFail
          onDisk <- versionsOnDisk(dir)
        } yield assert(outcome)(isSubtype[Outcome.PendingUpdate](anything)) &&
          assertTrue(onDisk.isEmpty)
      },
      test("full lifecycle: write genesis, then up-to-date, then gated incompatible, then allowed") {
        for {
          dir <- tempDir

          // 1. allowed update writes the genesis migration
          wrote1 <- MigrationCheck.check(dir, v1, updateAllowed).orFail
          afterGenesis <- versionsOnDisk(dir)

          // 2. re-running with the same tables is now up to date
          upToDate <- MigrationCheck.check(dir, v1, updateAllowed).orFail

          // 3. an incompatible change without the incompatible flag is blocked (not written)
          blocked <- MigrationCheck.check(dir, v2, updateAllowed).orFail
          afterBlocked <- versionsOnDisk(dir)

          // 4. with the incompatible flag it is written as a major bump
          wrote2 <- MigrationCheck.check(dir, v2, incompatibleAllowed).orFail
          afterMajor <- versionsOnDisk(dir)
        } yield assert(wrote1)(isSubtype[Outcome.Wrote](anything)) &&
          assert(upToDate)(equalTo(Outcome.UpToDate)) &&
          assert(blocked)(isSubtype[Outcome.BlockedIncompatible](anything)) &&
          assert(wrote2)(isSubtype[Outcome.Wrote](anything)) &&
          assertTrue(
            afterGenesis == Seq("1.0.0"),
            afterBlocked == Seq("1.0.0"),
            afterMajor == Seq("1.0.0", "2.0.0"),
          )
      },
    )

}
