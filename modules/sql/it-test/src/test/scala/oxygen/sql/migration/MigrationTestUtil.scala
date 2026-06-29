package oxygen.sql.migration

import java.nio.file.Files
import oxygen.sql.migration.MigrationGenerator.GenerateResult
import oxygen.sql.migration.model.MigrationState
import oxygen.sql.migration.persistence.MigrationFs
import oxygen.sql.migration.persistence.model.PersistedMigrationFile
import oxygen.sql.schema.TableRepr
import oxygen.zio.system.Path
import scala.collection.immutable.ArraySeq
import zio.*

/**
  * Test helpers for the FS-first migration world: stage migration files on disk so specs that just
  * need a schema (the query specs) can keep a one-line setup.
  */
object MigrationTestUtil {

  /** A fresh, empty temp directory as a [[Path]]. */
  val tempDir: UIO[Path] =
    for {
      tmp <- ZIO.attempt(Files.createTempDirectory("oxygen-it-migration")).orDie
      dir <- Path.of(tmp.toString).orDie
    } yield dir

  /** Generate the genesis migration file (empty -> these tables). */
  def genesisFile(tables: TableRepr[?]*): UIO[PersistedMigrationFile] =
    for {
      target <- ZIO.fromEither(MigrationState.fromTables(ArraySeq.from(tables))).orDieWith(e => new RuntimeException(e.toString))
      file <- MigrationGenerator.generate(None, target) match
        case Right(GenerateResult.Generated(file, _)) => ZIO.succeed(file)
        case Right(GenerateResult.UpToDate)           => ZIO.dieMessage("expected a genesis migration, got UpToDate")
        case Left(error)                              => ZIO.die(new RuntimeException(error.toString))
    } yield file

  /** A MigrationConfig pointing at a fresh temp dir that already contains a genesis file for the given tables. */
  def stagedConfigLayer(tables: TableRepr[?]*): ULayer[MigrationConfig] =
    ZLayer.fromZIO {
      for {
        dir <- tempDir
        file <- genesisFile(tables*)
        _ <- MigrationFs(dir).write(file).orDieWith(e => new RuntimeException(e.toString))
      } yield MigrationConfig(dir.toString, MigrationConfig.Atomicity.AllOrNothing)
    }

}
