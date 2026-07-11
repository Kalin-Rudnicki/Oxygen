package oxygen.example.webServer

import oxygen.example.db.ExampleSchema
import oxygen.sql.migration.model.MigrationSchema
import oxygen.sql.test.DbMigrationSpec

/**
  * Guards the example app's committed migration files against [[ExampleSchema]].
  *
  * Run with `OXYGEN_MIGRATION_ALLOW_UPDATE=true` after changing the model classes to regenerate the
  * files under [[migrationPath]] (which the app's `local.json` points its `migrationsDir` at).
  */
object ExampleDbMigrationSpec extends DbMigrationSpec {

  override def migrationPath: String = "example/apps/web-server/migrations"

  override def schema: MigrationSchema = ExampleSchema.schema

}
