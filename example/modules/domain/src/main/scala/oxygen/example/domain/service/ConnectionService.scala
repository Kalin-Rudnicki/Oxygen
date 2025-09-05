package oxygen.example.domain.service

import oxygen.example.core.model.user.*
import oxygen.example.domain.model.connection.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.*
import zio.*

final class ConnectionService(
    userService: UserService,
    connectionRepo: ConnectionRepo,
) {

  def getConnection(current: SimpleUser, other: UserId): IO[DomainError.UserIdDoesNotExist | DomainError.UserIsNotAConnection, UserConnection] =
    for {
      other <- userService.getUser(other)
      connection <- connectionRepo.findConnection(current.id, other.id).someOrElseZIO {
        ZIO.logWarning(s"$current is not connected with $other") *>
          ZIO.fail(DomainError.UserIsNotAConnection(current.id, other.id))
      }
    } yield UserConnection(
      current = current,
      other = other,
      createdAt = connection.createdAt,
    )

  def ensureAccess(current: SimpleUser, other: UserId): IO[DomainError.UserIdDoesNotExist | DomainError.UserIsNotAConnection, UserConnection] =
    if (current.id == other) ZIO.succeed(UserConnection(current, current, current.createdAt))
    else getConnection(current, other)

  def getConnectedUsers(userId: UserId): UIO[Seq[SimpleUser]] =
    connectionRepo.getConnectedUsers(userId)

  def requestConnection(current: SimpleUser, other: UserId): IO[ConnectionError, Unit] =
    for {
      now <- Clock.instant
      other <- userService.getUser(other).mapError { e => ConnectionError.UserIdDoesNotExist(e.id) }
      existingConnection <- connectionRepo.findConnection(current.id, other.id)
      _ <- ZIO.fail(ConnectionError.UserIsAlreadyAConnection(current.id, other.id)).whenDiscard(existingConnection.nonEmpty)
      existingOutgoingRequest <- connectionRepo.findConnectionRequest(current.id, other.id)
      _ <- ZIO.fail(ConnectionError.UserIsAlreadyAConnection(current.id, other.id)).whenDiscard(existingOutgoingRequest.nonEmpty)
      existingIncomingRequest <- connectionRepo.findConnectionRequest(other.id, current.id)
      _ <-
        if (existingIncomingRequest.nonEmpty)
          connectionRepo.connect(current.id, other.id, now) *>
            ZIO.logInfo(s"$other already requested connection with $current, connecting instead of requesting")
        else
          connectionRepo.createConnectionRequest(ConnectionRequest(current.id, other.id, now)) *>
            ZIO.logInfo(s"$current has requested connection with $other")
    } yield ()

  def acceptConnection(current: SimpleUser, other: UserId): IO[ConnectionError, Unit] =
    for {
      now <- Clock.instant
      other <- userService.getUser(other).mapError { e => ConnectionError.UserIdDoesNotExist(e.id) }
      existingIncomingRequest <- connectionRepo.findConnectionRequest(other.id, current.id)
      _ <- ZIO.fail(ConnectionError.NoConnectionRequest(current.id, other.id)).whenDiscard(existingIncomingRequest.isEmpty)
      _ <- connectionRepo.connect(current.id, other.id, now)
      _ <- ZIO.logInfo(s"$current has accepted connection request with $other")
    } yield ()

  def rejectConnection(current: SimpleUser, other: UserId): IO[ConnectionError, Unit] =
    for {
      other <- userService.getUser(other).mapError { e => ConnectionError.UserIdDoesNotExist(e.id) }
      existingIncomingRequest <- connectionRepo.findConnectionRequest(other.id, current.id)
      _ <- ZIO.fail(ConnectionError.NoConnectionRequest(current.id, other.id)).whenDiscard(existingIncomingRequest.isEmpty)
      _ <- connectionRepo.disconnect(current.id, other.id)
      _ <- ZIO.logInfo(s"$current has rejected connection request with $other")
    } yield ()

}
object ConnectionService {

  val layer: URLayer[ConnectionRepo & UserService, ConnectionService] =
    ZLayer.fromFunction { ConnectionService.apply }

}
