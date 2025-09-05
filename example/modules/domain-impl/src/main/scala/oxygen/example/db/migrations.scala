package oxygen.example.db

import oxygen.example.db.model.legacy.*
import oxygen.sql.migration.model.*

object migrations {

  lazy val migrations: Migrations =
    Migrations(
      v1,
      v2,
    )

  val v1: PlannedMigration =
    PlannedMigration.auto(1)(
      UserRow.v1.tableRepr,
      ConnectionRow.v1.tableRepr,
      ConnectionRequestRow.v1.tableRepr,
    )

  val v2: PlannedMigration =
    PlannedMigration.auto(2)(
      UserRow.v1.tableRepr,
      ConnectionRow.v1.tableRepr,
      ConnectionRequestRow.v1.tableRepr,
      PostRow.v1.tableRepr,
      CommentRow.v1.tableRepr,
    )

}
