package oxygen.example.domain.repo

import java.time.Instant
import oxygen.example.core.model.user.UserId
import oxygen.example.domain.model.connection.*
import oxygen.example.domain.model.user.SimpleUser
import zio.*

trait ConnectionRepo {

  def findConnection(current: UserId, other: UserId): UIO[Option[Connection]]
  def findConnectionRequest(current: UserId, other: UserId): UIO[Option[ConnectionRequest]]

  def getConnectedUsers(userId: UserId): UIO[Seq[SimpleUser]]

  def createConnectionRequest(connectionRequest: ConnectionRequest): UIO[Unit]

  def connect(current: UserId, other: UserId, now: Instant): UIO[Unit]
  def disconnect(current: UserId, other: UserId): UIO[Unit]

}
