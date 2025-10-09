package oxygen.sql

import java.util.UUID
import oxygen.sql.error.ConnectionError
import zio.*

final case class Connection(id: UUID, connection: java.sql.Connection)
object Connection {

  def wrapUnsafeJdbc(get: => java.sql.Connection): ZIO[Scope, ConnectionError, Connection] =
    (for {
      id <- Random.nextUUID
      connection <- ZIO.attempt { get }.mapError(ConnectionError(_))
      _ <- ZIO.addFinalizer { ZIO.attempt { connection.close() }.mapError(ConnectionError(_)).orDie }

      // TODO (KR) : figure out auto-commit
    } yield Connection(id, connection)).uninterruptible

}
