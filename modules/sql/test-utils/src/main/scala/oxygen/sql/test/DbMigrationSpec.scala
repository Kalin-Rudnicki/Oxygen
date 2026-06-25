package oxygen.sql.test

import oxygen.predef.test.*
import oxygen.sql.migration.MigrationCheck
import oxygen.sql.migration.MigrationCheck.{Config, Outcome}
import oxygen.sql.migration.model.MigrationSchema
import oxygen.zio.system.Path
import zio.*

/**
  * Reusable base spec implementing the "avro paradigm" for database migrations: it diffs the
  * current-code [[schema]] against the migration files committed at [[migrationPath]] and:
  *   - passes when they are up to date,
  *   - writes the new migration file (and passes) when `OXYGEN_MIGRATION_ALLOW_UPDATE` is set,
  *   - otherwise FAILS the build with the pending migration -- so CI catches a stale/uncommitted
  *     schema change.
  *
  * This check is database-free (it compares code against the filesystem). A breaking change
  * additionally requires `OXYGEN_MIGRATION_ALLOW_INCOMPATIBLE`.
  *
  * {{{
  * object MyDbMigrationSpec extends DbMigrationSpec {
  *   override def migrationPath: String = "modules/my-app/src/main/resources/migrations"
  *   override def schema: MigrationSchema = MigrationSchema.of(UserRow.tableRepr, PostRow.tableRepr)
  * }
  * }}}
  */
abstract class DbMigrationSpec extends OxygenSpecDefault {

  /** Filesystem directory where the migration files are committed. Resolved from the working dir. */
  def migrationPath: String

  /** The current-code schema (the latest set of tables). */
  def schema: MigrationSchema

  override final def testSpec: TestSpec =
    suite("DbMigrationSpec")(
      test("committed migrations are up to date with the current-code schema") {
        for {
          dir <- Path.of(migrationPath).orDie
          config <- Config.fromEnv.orDie
          outcome <- MigrationCheck.check(dir, schema, config).orDieWith(e => new RuntimeException(e.toString))
        } yield outcome match
          case Outcome.UpToDate =>
            assertCompletes
          case Outcome.Wrote(file, _) =>
            assertCompletes.label(s"wrote migration ${file.version} -- review and commit it")
          case Outcome.PendingUpdate(file, compatibility) =>
            assertTrue(false).label(
              s"Committed migrations are out of date: would create ${file.version} ($compatibility). " +
                s"Re-run with ${Config.allowUpdateEnv}=true to generate it.",
            )
          case Outcome.BlockedIncompatible(file) =>
            assertTrue(false).label(
              s"Migration ${file.version} is incompatible (data-loss / breaking). " +
                s"Re-run with ${Config.allowUpdateEnv}=true and ${Config.allowIncompatibleEnv}=true to allow it.",
            )
      } @@ TestAspect.withLiveEnvironment, // Config.fromEnv must read the real OS env, not zio-test's empty TestSystem
    )

}
