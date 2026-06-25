package oxygen.sql.migration

import java.util.UUID
import oxygen.sql.migration.model.MigrationSchema
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*
import oxygen.sql.test.DbMigrationSpec

/**
  * A concrete, runnable example of the avro-style managed-migration workflow.
  *
  * The migration files live under [[migrationPath]] and are committed to the repo. In CI this spec
  * asserts they are up to date with [[schema]]; locally, re-run with `OXYGEN_MIGRATION_ALLOW_UPDATE=true`
  * to (re)generate them after changing the model classes below.
  */
object ManagedMigrationSpec extends DbMigrationSpec {

  @schemaName("managed")
  @tableName("widget")
  final case class Widget(
      @primaryKey id: UUID,
      name: String,
      description: Option[String],
  )
  object Widget extends TableCompanion[Widget, UUID](TableRepr.derived[Widget])

  @schemaName("managed")
  @tableName("gadget")
  final case class Gadget(
      @primaryKey id: Int,
      label: String,
  )
  object Gadget extends TableCompanion[Gadget, Int](TableRepr.derived[Gadget])

  override def migrationPath: String = "modules/sql/it-test/src/test/resources/db-migration-managed"

  override def schema: MigrationSchema = MigrationSchema.of(Widget, Gadget)

}
