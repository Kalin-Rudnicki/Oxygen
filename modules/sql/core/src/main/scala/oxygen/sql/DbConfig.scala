package oxygen.sql

import oxygen.json.JsonCodec
import oxygen.predef.core.*
import oxygen.zio.instances.given
import zio.*

final case class DbConfig(
    target: DbConfig.Target,
    credentials: Option[DbConfig.Credentials],
    pool: DbConfig.Pool,
    logging: DbConfig.Logging,
    execution: DbConfig.Execution,
) derives JsonCodec
object DbConfig {

  final case class Target(
      database: String,
      host: String,
      port: Int,
  ) derives JsonCodec {
    def jdbcUrl(dbUrlPrefix: String): String = s"jdbc:$dbUrlPrefix://$host:$port/$database"
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

  /**
    * @param bufferChunkSize Size of the chunk to read from JDBC result set
    * @param bufferNumChunks Optional buffering of chunks.
    */
  final case class Execution(
      bufferChunkSize: NonEmptyList[Int],
      bufferNumChunks: Option[Int],
  ) derives JsonCodec
  object Execution {

    val default: Execution =
      Execution(
        bufferChunkSize = NonEmptyList.of(16, 64, 64, 256, 256, 2048),
        bufferNumChunks = 2.some,
      )

  }

}
