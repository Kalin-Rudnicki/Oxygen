package oxygen.sql.migration

import oxygen.json.JsonCodec
import oxygen.predef.core.*
import zio.{ULayer, ZLayer}

final case class MigrationConfig(
    atomicity: MigrationConfig.Atomicity,
) derives JsonCodec
object MigrationConfig {

  val default: MigrationConfig = MigrationConfig(Atomicity.AllOrNothing)
  val defaultLayer: ULayer[MigrationConfig] = ZLayer.succeed(default)

  /**
    * None : (Highly not recommended) Will run your migration outside a transaction.
    *      : If there ends up being an error within a singular migration, your DB will be in a very weird state.
    * PerMigration : Will run each migration in its own transaction.
    *              : This means that if you need to run multiple migrations (A and B), and A succeeds, but B fails, your DB will be in the state between A and B.
    * AllOrNothing : Will run the entire migration in its own transaction.
    *              : This means that if you need to run multiple migrations (A and B), and A succeeds, but B fails, your DB will roll back to its state before A.
    */
  enum Atomicity extends Enum[Atomicity] {
    case None
    case PerMigration
    case AllOrNothing
  }
  object Atomicity extends Enum.Companion[Atomicity]

}
