package oxygen.example.domain.repo

import oxygen.example.core.model.user.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.user.*
import zio.*

trait UserRepo {

  def findUserByEmail(email: Email): UIO[Option[FullUser]]

  def findUserById(id: UserId): UIO[Option[FullUser]]

  def insertUser(user: FullUser): UIO[Unit]

  final def getUserById(id: UserId): IO[DomainError.UserIdDoesNotExist, FullUser] =
    findUserById(id).someOrFail(DomainError.UserIdDoesNotExist(id))

}
