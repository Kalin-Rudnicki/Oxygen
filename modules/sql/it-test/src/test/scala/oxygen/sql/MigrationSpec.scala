package oxygen.sql

import oxygen.core.Version
import oxygen.predef.test.*
import oxygen.sql.error.QueryError
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import oxygen.sql.migration.persistence.model.PersistedMigrationFile
import oxygen.sql.query.*
import oxygen.sql.schema.*
import scala.collection.immutable.ArraySeq

object MigrationSpec extends OxygenSpec[Database & MigrationService] {

  @tableName("model_a")
  final case class ModelA1(
      @primaryKey id: Int,
      field1: String,
      field2: Boolean,
  )
  object ModelA1 extends TableCompanion[ModelA1, Int](TableRepr.derived[ModelA1])

  // adds a nullable column -> backwards-compatible follow-up migration
  @tableName("model_a")
  final case class ModelA2(
      @primaryKey id: Int,
      field1: String,
      field2: Boolean,
      other: Option[Int],
  )
  object ModelA2 extends TableCompanion[ModelA2, Int](TableRepr.derived[ModelA2])

  private def stateOf(tables: TableRepr[?]*): MigrationState =
    MigrationState.fromTables(ArraySeq.from(tables)) match
      case Right(s)    => s
      case Left(error) => throw new RuntimeException(error.toString)

  private def fileFor(previous: Option[(Version, MigrationState)], tables: TableRepr[?]*): PersistedMigrationFile =
    MigrationGenerator.generate(previous, stateOf(tables*)) match
      case Right(MigrationGenerator.GenerateResult.Generated(file, _)) => file
      case other                                                       => throw new RuntimeException(s"expected a generated migration, got: $other")

  private val v1: PersistedMigrationFile = fileFor(None, ModelA1.tableRepr)
  private val v2: PersistedMigrationFile = fileFor(Some((Version("1.0.0"), stateOf(ModelA1.tableRepr))), ModelA2.tableRepr)

  override def testSpec: TestSpec =
    suite("MigrationSpec")(
      test("applies genesis, is idempotent, and preserves data across a re-run") {
        for {
          before <- ModelA1.selectAll.execute().arraySeq.exit
          res1 <- MigrationService.applyMigrations(ArraySeq(v1))
          _ <- ModelA1.insert.all(ModelA1(1, "a", true), ModelA1(2, "b", false)).unit
          got1 <- ModelA1.selectAll.execute().to[Seq]
          res2 <- MigrationService.applyMigrations(ArraySeq(v1))
          got2 <- ModelA1.selectAll.execute().to[Seq]
        } yield assert(before)(failsWithA[QueryError]) &&
          assertTrue(
            res1.executed.length == 1,
            res2.executed.isEmpty,
            got1.sortBy(_.id) == Seq(ModelA1(1, "a", true), ModelA1(2, "b", false)),
            got2.sortBy(_.id) == got1.sortBy(_.id),
          )
      },
      test("applies a follow-up migration, preserving existing rows") {
        for {
          _ <- MigrationService.applyMigrations(ArraySeq(v1))
          _ <- ModelA1.insert.all(ModelA1(1, "a", true)).unit
          res <- MigrationService.applyMigrations(ArraySeq(v1, v2))
          got <- ModelA2.selectAll.execute().to[Seq]
        } yield assertTrue(
          res.executed.length == 1,
          res.executed.headOption.map(_.version) == Some(Version("1.1.0")),
          got == Seq(ModelA2(1, "a", true, None)),
        )
      },
      test("an executed migration absent from the filesystem -> MissingMigration") {
        for {
          _ <- MigrationService.applyMigrations(ArraySeq(v1))
          err <- MigrationService.applyMigrations(ArraySeq.empty).exit
        } yield assert(err)(failsWithA[MigrationError.MissingMigration])
      },
      test("a filesystem migration that diverges from what was executed -> MigrationsDiffer") {
        // same version as v1, but a different diff (genesis for the 4-column table)
        val v1Diverged = fileFor(None, ModelA2.tableRepr).copy(version = "1.0.0")
        for {
          _ <- MigrationService.applyMigrations(ArraySeq(v1))
          err <- MigrationService.applyMigrations(ArraySeq(v1Diverged)).exit
        } yield assert(err)(failsWithA[MigrationError.MigrationsDiffer])
      },
      test("verifyAgainst succeeds when the committed migrations match the current-code tables") {
        for {
          res <- MigrationService.applyMigrations(ArraySeq(v1), MigrationSchema.of(ModelA1.tableRepr))
        } yield assertTrue(res.executed.length == 1)
      },
      test("verifyAgainst fails fast with MigrationsStale when code has drifted ahead of the filesystem") {
        for {
          err <- MigrationService.applyMigrations(ArraySeq(v1), MigrationSchema.of(ModelA2.tableRepr)).exit
        } yield assert(err)(failsWithA[MigrationError.MigrationsStale])
      },
    )

  // deliberately not shared: each test runs against its own fresh database
  override def layerProvider: LayerProvider[R] =
    LayerProvider.providePerTest[Env](
      Helpers.databaseLayer,
      Helpers.testContainerLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.withLiveRandom, TestAspect.withLiveClock)

}
