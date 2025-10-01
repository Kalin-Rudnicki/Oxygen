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

  def acquire(url: String): ZIO[Scope, ConnectionError, Connection] =
    ZIO.logWarning("Attempting to connect to database without credentials") *>
      wrapUnsafe { java.sql.DriverManager.getConnection(url) }

  def acquire(url: String, username: String, password: String): ZIO[Scope, ConnectionError, Connection] =
    wrapUnsafe { java.sql.DriverManager.getConnection(url, username, password) }

  def acquire(config: DbConfig): ZIO[Scope, ConnectionError, Connection] = config.credentials match
    case Some(credentials) => acquire(config.target.jdbcUrl, credentials.username, credentials.password)
    case None              => acquire(config.target.jdbcUrl)

}
