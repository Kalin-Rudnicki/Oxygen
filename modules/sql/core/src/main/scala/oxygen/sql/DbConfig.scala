package oxygen.sql

import oxygen.json.jsonSecret
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.zio.instances.given
import zio.*

final case class DbConfig(
    target: DbConfig.Target,
    credentials: Nullable[DbConfig.Credentials],
    pool: DbConfig.Pool = DbConfig.Pool.default,
    logging: DbConfig.Logging = DbConfig.Logging.default,
    execution: DbConfig.Execution = DbConfig.Execution.default,
) derives JsonSchema
object DbConfig {

  final case class Target(
      database: String,
      host: String,
      port: Int,
  ) derives JsonSchema {
    def jdbcUrl(dbUrlPrefix: String): String = s"jdbc:$dbUrlPrefix://$host:$port/$database"
  }

  @jsonSecret
  final case class Credentials(
      username: String,
      password: String,
  ) derives JsonSchema

  final case class Pool(
      minConnections: Int = 2,
      maxConnections: Int = 8,
      duration: Duration = 5.minutes,
  ) derives JsonSchema
  object Pool {
    val default: Pool = Pool()
  }

  final case class Logging(
      queryLogLevel: LogLevel = LogLevel.Trace,
      logSql: Boolean = true,
  ) derives JsonSchema
  object Logging {
    val default: Logging = Logging()
  }

  /**
    * @param bufferChunkSize Size of the chunk to read from JDBC result set
    * @param bufferNumChunks Optional buffering of chunks.
    */
  final case class Execution(
      bufferChunkSize: NonEmptyList[Int] = NonEmptyList.of(16, 64, 64, 256, 256, 2048),
      bufferNumChunks: Option[Int] = 2.some,
  ) derives JsonSchema
  object Execution {
    val default: Execution = Execution()
  }

}
