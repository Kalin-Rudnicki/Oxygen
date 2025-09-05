package oxygen.example.domain.model.error

import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*

sealed trait DomainError
object DomainError {

  final case class UserIdDoesNotExist(id: UserId) extends DomainError
  final case class UserIsNotAConnection(current: UserId, other: UserId) extends DomainError

  final case class PostIdDoesNotExist(id: PostId) extends DomainError

}
