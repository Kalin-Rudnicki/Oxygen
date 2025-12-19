package oxygen.example.domain.repo

import oxygen.example.core.model.user.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.user.*
import oxygen.storage.CRUDRepo
import zio.*

trait UserRepo extends CRUDRepo[UserId, FullUser] {

  def findUserByEmail(email: Email): UIO[Option[FullUser]]

  final def getUserById(id: UserId): IO[DomainError.UserIdDoesNotExist, FullUser] =
    findByKey(id).someOrFail(DomainError.UserIdDoesNotExist(id))

}
