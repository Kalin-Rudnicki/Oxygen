package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.user.*
import oxygen.sql.*
import oxygen.sql.query.{PostgresCRUDRepo, TableCompanion}
import zio.*

final case class PostgresUserRepo(
    db: Database,
) extends UserRepo,
      PostgresCRUDRepo.MapInfallible[UserId, FullUser] {

  override protected type DbA = UserRow
  override protected type DbK = UserId

  override protected val companion: TableCompanion[UserRow, UserId] = UserRow

  override protected def keyToDb(key: UserId): UserId = key
  override protected def valueToDb(value: FullUser): UserRow = value.toDb
  override protected def valueToDomain(value: UserRow): FullUser = value.toDomain

  override def findUserByEmail(email: Email): UIO[Option[FullUser]] =
    UserRow.userByEmail.map(_.toDomain).execute(email.referenceEmail).option.orDie.usingDb(db)

}
object PostgresUserRepo {

  val layer: URLayer[Database, PostgresUserRepo] =
    ZLayer.fromFunction { PostgresUserRepo.apply }

}
