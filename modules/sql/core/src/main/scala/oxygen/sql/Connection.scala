package oxygen.sql

import java.util.UUID
import oxygen.sql.error.ConnectionError
import zio.*

final case class Connection(id: UUID, connection: java.sql.Connection) {

  private[sql] def disableAutoCommitScoped: ZIO[Scope, ConnectionError, Unit] =
    ZIO
      .attempt { connection.setAutoCommit(false) }
      .mapError(ConnectionError(_))
      .withFinalizer { _ => ZIO.attempt { connection.setAutoCommit(true) }.orDieWith(ConnectionError(_)) }

}
object Connection {

  def wrapUnsafeJdbc(get: => java.sql.Connection): ZIO[Scope, ConnectionError, Connection] =
    (for {
      id <- Random.nextUUID
      connection <-
        ZIO
          .attempt { get }
          .mapError(ConnectionError(_))
          .withFinalizer { connection => ZIO.attempt { connection.close() }.orDieWith(ConnectionError(_)) }
    } yield Connection(id, connection)).uninterruptible

}
