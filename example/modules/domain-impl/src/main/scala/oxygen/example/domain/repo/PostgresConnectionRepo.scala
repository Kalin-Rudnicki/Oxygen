package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.connection.*
import oxygen.example.domain.model.user.*
import oxygen.sql.{Connection as _, *}
import oxygen.sql.query.{PostgresCRUDRepo, TableCompanion}
import oxygen.storage.CRUDRepo
import zio.*

final case class PostgresConnectionRepo(
    db: Database,
    atomically: Atomically,
) extends ConnectionRepo { self =>

  private object PostgresConnection extends PostgresCRUDRepo.MapInfallible[Connection, (UserId, UserId)] {

    override val db: Database = self.db

    override protected type DbA = ConnectionRow
    override protected type DbK = (UserId, UserId)

    override protected val companion: TableCompanion[ConnectionRow, (UserId, UserId)] = ConnectionRow

    override protected def keyToDb(key: (UserId, UserId)): (UserId, UserId) = key
    override protected def valueToDb(value: Connection): ConnectionRow = value.toDb
    override protected def valueToDomain(value: ConnectionRow): Connection = value.toDomain

  }

  private object PostgresConnectionRequest extends PostgresCRUDRepo.MapInfallible[ConnectionRequest, (UserId, UserId)] {

    override val db: Database = self.db

    override protected type DbA = ConnectionRequestRow
    override protected type DbK = (UserId, UserId)

    override protected val companion: TableCompanion[ConnectionRequestRow, (UserId, UserId)] = ConnectionRequestRow

    override protected def keyToDb(key: (UserId, UserId)): (UserId, UserId) = key
    override protected def valueToDb(value: ConnectionRequest): ConnectionRequestRow = value.toDb
    override protected def valueToDomain(value: ConnectionRequestRow): ConnectionRequest = value.toDomain

  }

  override protected val connection: CRUDRepo[Connection, (UserId, UserId)] = PostgresConnection
  override protected val connectionRequest: CRUDRepo[ConnectionRequest, (UserId, UserId)] = PostgresConnectionRequest

  override def getConnectedUsers(userId: UserId): UIO[Seq[SimpleUser]] =
    UserRow.getConnections.map(_.toDomain.toSimple).execute(userId).to[Seq].orDie.usingDb(db)

}
object PostgresConnectionRepo {

  val layer: URLayer[Database & Atomically, ConnectionRepo] =
    ZLayer.fromFunction { PostgresConnectionRepo.apply }

}
