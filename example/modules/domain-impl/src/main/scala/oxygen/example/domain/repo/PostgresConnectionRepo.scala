package oxygen.example.domain.repo

import java.time.Instant
import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.connection.*
import oxygen.example.domain.model.user.*
import oxygen.sql.{Connection as _, *}
import zio.*

final case class PostgresConnectionRepo(
    db: Database,
    atomically: Atomically,
) extends ConnectionRepo {

  override def findConnection(current: UserId, other: UserId): UIO[Option[Connection]] =
    ConnectionRow.selectByPK.map(_.toDomain).execute(current, other).option.orDie.usingDb(db)

  override def findConnectionRequest(current: UserId, other: UserId): UIO[Option[ConnectionRequest]] =
    ConnectionRequestRow.selectByPK.map(_.toDomain).execute(current, other).option.orDie.usingDb(db)

  override def getConnectedUsers(userId: UserId): UIO[Seq[SimpleUser]] =
    UserRow.getConnections.map(_.toDomain.toSimple).execute(userId).to[Seq].orDie.usingDb(db)

  override def createConnectionRequest(connectionRequest: ConnectionRequest): UIO[Unit] =
    ConnectionRequestRow.insert(connectionRequest.toDb).unit.orDie.usingDb(db)

  override def connect(current: UserId, other: UserId, now: Instant): UIO[Unit] =
    atomically {
      for {
        _ <- ConnectionRequestRow.deleteByPK.all((current, other), (other, current)).unit
        _ <- ConnectionRow.insert.all(ConnectionRow(current, other, now), ConnectionRow(other, current, now)).unit
      } yield ()
    }.orDie.usingDb(db)

  override def disconnect(current: UserId, other: UserId): UIO[Unit] =
    atomically {
      for {
        _ <- ConnectionRequestRow.deleteByPK.all((current, other), (other, current)).unit
        _ <- ConnectionRow.deleteByPK.all((current, other), (other, current)).unit
      } yield ()
    }.orDie.usingDb(db)

}
object PostgresConnectionRepo {

  val layer: URLayer[Database & Atomically, ConnectionRepo] =
    ZLayer.fromFunction { PostgresConnectionRepo.apply }

}
