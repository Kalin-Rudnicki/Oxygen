package oxygen.sql

import oxygen.json.JsonCodec
import oxygen.zio.instances.given
import zio.*

final case class DbConfig(
    target: DbConfig.Target,
    credentials: Option[DbConfig.Credentials],
    pool: DbConfig.Pool,
    logging: DbConfig.Logging,
) derives JsonCodec
object DbConfig {

  final case class Target(
      database: String,
      host: String,
      port: Int,
  ) derives JsonCodec {
    val jdbcUrl: String = s"jdbc:postgresql://$host:$port/$database"
  }

  final case class Credentials(
      username: String,
      password: String,
  ) derives JsonCodec

  final case class Pool(
      minConnections: Int,
      maxConnections: Int,
      duration: Duration,
  ) derives JsonCodec

  final case class Logging(
      queryLogLevel: LogLevel,
      logSql: Boolean,
  ) derives JsonCodec

}
