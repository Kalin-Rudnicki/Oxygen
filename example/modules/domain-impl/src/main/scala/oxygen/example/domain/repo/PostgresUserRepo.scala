package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.user.*
import oxygen.sql.*
import zio.*

final case class PostgresUserRepo(
    db: Database,
) extends UserRepo {

  override def findUserByEmail(email: Email): UIO[Option[FullUser]] =
    UserRow.userByEmail.map(_.toDomain).execute(email.referenceEmail).option.orDie.usingDb(db)

  override def findUserById(id: UserId): UIO[Option[FullUser]] =
    UserRow.selectByPK.map(_.toDomain).execute(id).option.orDie.usingDb(db)

  override def insertUser(user: FullUser): UIO[Unit] =
    UserRow.insert.execute(user.toDb).unit.orDie.usingDb(db)

}
object PostgresUserRepo {

  val layer: URLayer[Database, PostgresUserRepo] =
    ZLayer.fromFunction { PostgresUserRepo.apply }

}
