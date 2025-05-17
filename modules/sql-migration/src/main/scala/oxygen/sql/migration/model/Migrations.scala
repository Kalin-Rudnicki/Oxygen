package oxygen.sql.migration.model

import oxygen.predef.core.*

final case class Migrations(migrations: Contiguous[PlannedMigration])
object Migrations {

  def apply(migrations: PlannedMigration*): Migrations =
    Migrations(migrations.toContiguous)

}
