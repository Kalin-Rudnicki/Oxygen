package oxygen.sql.migration

import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import zio.{ULayer, ZLayer}

final case class MigrationConfig(
    migrationsDir: String,
    atomicity: MigrationConfig.Atomicity = MigrationConfig.Atomicity.AllOrNothing,
) derives JsonSchema
object MigrationConfig {

  val default: MigrationConfig = MigrationConfig("migrations", Atomicity.AllOrNothing)
  val defaultLayer: ULayer[MigrationConfig] = ZLayer.succeed(default)

  def layer(migrationsDir: String, atomicity: Atomicity = Atomicity.AllOrNothing): ULayer[MigrationConfig] =
    ZLayer.succeed(MigrationConfig(migrationsDir, atomicity))

  /**
    * None : (Highly not recommended) Will run your migration outside a transaction.
    *      : If there ends up being an error within a singular migration, your DB will be in a very weird state.
    * PerMigration : Will run each migration in its own transaction.
    *              : This means that if you need to run multiple migrations (A and B), and A succeeds, but B fails, your DB will be in the state between A and B.
    * AllOrNothing : Will run the entire migration in its own transaction.
    *              : This means that if you need to run multiple migrations (A and B), and A succeeds, but B fails, your DB will roll back to its state before A.
    */
  enum Atomicity derives StrictEnum {
    case None
    case PerMigration
    case AllOrNothing
  }

}
