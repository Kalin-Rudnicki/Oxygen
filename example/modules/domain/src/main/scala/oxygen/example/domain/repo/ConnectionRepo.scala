package oxygen.example.domain.repo

import java.time.Instant
import oxygen.example.core.model.user.UserId
import oxygen.example.domain.model.connection.*
import oxygen.example.domain.model.user.SimpleUser
import oxygen.storage.{Atomically, CRUDRepo}
import zio.*

trait ConnectionRepo {

  protected val atomically: Atomically

  // Intentionally protected. We DO want the benefit of the free queries, but we DON'T want to expose the raw functionality outside the repo.
  protected val connection: CRUDRepo[(UserId, UserId), Connection]
  protected val connectionRequest: CRUDRepo[(UserId, UserId), ConnectionRequest]

  def getConnectedUsers(userId: UserId): UIO[Seq[SimpleUser]]

  final def connect(current: UserId, other: UserId, now: Instant): UIO[Unit] =
    atomically {
      for {
        _ <- connectionRequest.deleteAll((current, other), (other, current))
        _ <- connection.insertAll(Connection(current, other, now), Connection(other, current, now))
      } yield ()
    }

  final def disconnect(current: UserId, other: UserId): UIO[Unit] =
    atomically {
      for {
        _ <- connectionRequest.deleteAll((current, other), (other, current))
        _ <- connection.deleteAll((current, other), (other, current))
      } yield ()
    }

  final def createConnectionRequest(req: ConnectionRequest): UIO[Unit] = connectionRequest.insert(req)

  final def findConnection(current: UserId, other: UserId): UIO[Option[Connection]] = connection.findByKey((current, other))
  final def findConnectionRequest(current: UserId, other: UserId): UIO[Option[ConnectionRequest]] = connectionRequest.findByKey((current, other))

}
