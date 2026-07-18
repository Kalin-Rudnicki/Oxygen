package oxygen.example.db

import oxygen.example.db.model.*
import oxygen.sql.migration.model.MigrationSchema

/**
  * The current-code database schema for the example app -- the single source of truth shared by the
  * app's startup migration and the migration guard spec.
  */
object ExampleSchema {

  val schema: MigrationSchema =
    MigrationSchema.of(
      UserRow,
      ConnectionRow,
      ConnectionRequestRow,
      PostRow,
      CommentRow,
      InitPaymentMethodRow,
      PaymentMethodRow,
      PaymentRow,
    )

}
