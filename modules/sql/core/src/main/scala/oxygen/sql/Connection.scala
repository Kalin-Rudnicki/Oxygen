package oxygen.sql

import java.util.UUID
import oxygen.sql.error.ConnectionError
import zio.*

private[sql] final case class Connection(id: UUID, connection: java.sql.Connection)
object Connection {

  private def wrapUnsafe(get: => java.sql.Connection): ZIO[Scope, ConnectionError, Connection] =
    (for {
      id <- Random.nextUUID
      connection <- ZIO.attempt { get }.mapError(ConnectionError(_))
      _ <- ZIO.addFinalizer { ZIO.attempt { connection.close() }.mapError(ConnectionError(_)).orDie }

      // TODO (KR) : figure out auto-commit
    } yield Connection(id, connection)).uninterruptible

  // this is done manually because for some reason `DriverManager` was not recognizing PSQL jdbc urls...
  private lazy val psqlDriver: org.postgresql.Driver = new org.postgresql.Driver()
  private def emptyProps: java.util.Properties = new java.util.Properties()
  private def credProps(username: String, password: String): java.util.Properties = {
    val p = emptyProps
    p.put("user", username)
    p.put("password", password)
    p
  }

  def acquire(url: String): ZIO[Scope, ConnectionError, Connection] =
    ZIO.logWarning("Attempting to connect to database without credentials") *>
      wrapUnsafe { psqlDriver.connect(url, emptyProps) }

  def acquire(url: String, username: String, password: String): ZIO[Scope, ConnectionError, Connection] =
    wrapUnsafe { psqlDriver.connect(url, credProps(username, password)) }

  def acquire(config: DbConfig): ZIO[Scope, ConnectionError, Connection] = config.credentials match
    case Some(credentials) => acquire(config.target.jdbcUrl, credentials.username, credentials.password)
    case None              => acquire(config.target.jdbcUrl)

}
