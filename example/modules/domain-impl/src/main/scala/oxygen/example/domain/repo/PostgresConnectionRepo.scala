package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.{given, *}
import oxygen.example.conversion.domainToDb.given
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.connection.*
import oxygen.example.domain.model.user.*
import oxygen.sql.{Connection as _, *}
import oxygen.sql.query.PostgresCRUDRepo
import oxygen.storage.CRUDRepo
import zio.*

final case class PostgresConnectionRepo(
    db: Database,
    atomically: Atomically,
) extends ConnectionRepo { self =>

  private object PostgresConnection extends PostgresCRUDRepo.TransformInfallible[(UserId, UserId), Connection, ConnectionRow](ConnectionRow) {

    override val db: Database = self.db

  }

  private object PostgresConnectionRequest extends PostgresCRUDRepo.TransformInfallible[(UserId, UserId), ConnectionRequest, ConnectionRequestRow](ConnectionRequestRow) {

    override val db: Database = self.db

  }

  override protected val connection: CRUDRepo[(UserId, UserId), Connection] = PostgresConnection
  override protected val connectionRequest: CRUDRepo[(UserId, UserId), ConnectionRequest] = PostgresConnectionRequest

  override def getConnectedUsers(userId: UserId): UIO[Seq[SimpleUser]] =
    UserRow.getConnections.map(_.toDomain.toSimple).execute(userId).to[Seq].orDie.usingDb(db)

}
object PostgresConnectionRepo {

  val layer: URLayer[Database & Atomically, ConnectionRepo] =
    ZLayer.fromFunction { PostgresConnectionRepo.apply }

}
