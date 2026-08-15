package oxygen.sql

import oxygen.core.typeclass.StrictEnum
import oxygen.json.jsonSecret
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.zio.instances.given
import zio.*

final case class DbConfig(
    target: DbConfig.Target,
    credentials: Nullable[DbConfig.Credentials],
    connection: DbConfig.Connection = DbConfig.Connection.default,
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

  /**
    * Optional JDBC connection settings, translated into the driver `Properties` alongside `user`/`password`.
    *
    * Every field is optional and defaults to "unset" ([[Connection.default]]), which preserves the historical
    * behavior of only supplying credentials. Property names/values below are Postgres (`pgjdbc`) flavored; other
    * dialects can specialize this translation later.
    *
    * @param sslMode        Postgres `sslmode` (see [[SslMode]]).
    * @param sslRootCert    Path to the trusted CA cert (`sslrootcert`) — typically required for `verify-ca`/`verify-full`.
    * @param sslCert        Path to the client cert (`sslcert`) for mutual TLS.
    * @param sslKey         Path to the client key file (`sslkey`) for mutual TLS. A file path, not the key material itself.
    * @param connectTimeout Max time to establish the TCP connection (`connectTimeout`, whole seconds).
    * @param socketTimeout  Max time a socket read may block (`socketTimeout`, whole seconds).
    * @param applicationName Reported to the server as `ApplicationName` (shows up in `pg_stat_activity`).
    * @param extraProperties Escape hatch: arbitrary driver properties copied verbatim. Applied last, so these
    *                        override the typed properties (and credentials) on key collision.
    */
  final case class Connection(
      sslMode: Option[SslMode] = None,
      sslRootCert: Option[String] = None,
      sslCert: Option[String] = None,
      sslKey: Option[String] = None,
      connectTimeout: Option[Duration] = None,
      socketTimeout: Option[Duration] = None,
      applicationName: Option[String] = None,
      extraProperties: Map[String, String] = Map.empty,
  ) derives JsonSchema {

    /** Typed settings translated into `(key, value)` JDBC property pairs, followed by the verbatim escape-hatch props. */
    def properties: Seq[(String, String)] =
      Seq(
        sslMode.map(m => "sslmode" -> m.sslmode),
        sslRootCert.map("sslrootcert" -> _),
        sslCert.map("sslcert" -> _),
        sslKey.map("sslkey" -> _),
        connectTimeout.map(d => "connectTimeout" -> d.toSeconds.toString),
        socketTimeout.map(d => "socketTimeout" -> d.toSeconds.toString),
        applicationName.map("ApplicationName" -> _),
      ).flatten ++ extraProperties.toSeq

  }
  object Connection {
    val default: Connection = Connection()
  }

  /**
    * Postgres `sslmode`. Encoded/decoded (case-insensitively) using the exact libpq spellings:
    * `disable, allow, prefer, require, verify-ca, verify-full`.
    *
    * See: https://jdbc.postgresql.org/documentation/ssl/ and the libpq `sslmode` docs.
    */
  enum SslMode(final val sslmode: String) {
    case Disable extends SslMode("disable")
    case Allow extends SslMode("allow")
    case Prefer extends SslMode("prefer")
    case Require extends SslMode("require")
    case VerifyCa extends SslMode("verify-ca")
    case VerifyFull extends SslMode("verify-full")
  }
  object SslMode {
    given StrictEnum[SslMode] = StrictEnum.derive[SslMode](_.sslmode)
    given JsonSchema[SslMode] = JsonSchema.fromPlainText
  }

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
