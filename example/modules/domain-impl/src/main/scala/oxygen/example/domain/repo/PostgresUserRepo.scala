package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.{given, *}
import oxygen.example.conversion.domainToDb.given
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.user.*
import oxygen.sql.*
import oxygen.sql.query.PostgresCRUDRepo
import zio.*

final case class PostgresUserRepo(
    db: Database,
) extends UserRepo,
      PostgresCRUDRepo.TransformInfallible[UserId, FullUser, UserRow](UserRow) {

  override def findUserByEmail(email: Email): UIO[Option[FullUser]] =
    UserRow.userByEmail.map(_.toDomain).execute(email.referenceEmail).option.orDie.usingDb(db)

}
object PostgresUserRepo {

  val layer: URLayer[Database, PostgresUserRepo] =
    ZLayer.fromFunction { PostgresUserRepo.apply }

}
