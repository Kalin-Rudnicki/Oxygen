package oxygen.sql.migration

import java.nio.file.Files
import oxygen.predef.test.*
import oxygen.sql.migration.persistence.MigrationFs
import oxygen.sql.migration.persistence.model.*
import oxygen.zio.system.Path
import scala.collection.immutable.ArraySeq
import zio.*

object MigrationFsSpec extends OxygenSpecDefault {

  private def fileFor(version: String, prev: Option[String]): PersistedMigrationFile =
    PersistedMigrationFile(
      formatVersion = PersistedMigrationFile.currentFormatVersion,
      version = version,
      previousVersion = prev,
      compatibility = MigrationCompatibility.BackwardsCompatible,
      diff = ArraySeq.empty,
      state = MigrationStateColumn(
        extensions = Set.empty,
        schemas = Set("public"),
        tables = ArraySeq.empty,
      ),
    )

  private val tempMigrationFs: ZIO[Any, Nothing, MigrationFs] =
    for {
      tmp <- ZIO.attempt(Files.createTempDirectory("oxygen-migration-fs")).orDie
      dir <- Path.of(tmp.toString).orDie
    } yield MigrationFs(dir)

  override def testSpec: TestSpec =
    suite("MigrationFsSpec")(
      test("empty directory lists nothing") {
        for {
          fs <- tempMigrationFs
          listed <- fs.list
          latest <- fs.latest
        } yield assertTrue(listed.isEmpty, latest.isEmpty)
      },
      test("write then list returns files ordered by version (not write order)") {
        for {
          fs <- tempMigrationFs
          _ <- fs.write(fileFor("1.0.0", None))
          _ <- fs.write(fileFor("1.2.0", Some("1.1.0")))
          _ <- fs.write(fileFor("1.1.0", Some("1.0.0")))
          listed <- fs.list
          latest <- fs.latest
        } yield assertTrue(
          listed.map(_.version.show) == ArraySeq("1.0.0", "1.1.0", "1.2.0"),
          latest.map(_.version.show).contains("1.2.0"),
        )
      },
      test("written file round-trips back through read") {
        val original = fileFor("2.0.0", Some("1.2.0"))
        for {
          fs <- tempMigrationFs
          _ <- fs.write(original)
          listed <- fs.list
        } yield assertTrue(listed.map(_.file) == ArraySeq(original))
      },
    )

}
