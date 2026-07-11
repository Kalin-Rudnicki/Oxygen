package oxygen.example.domain.model.error

import oxygen.example.core.model.*

sealed trait ConnectionError
object ConnectionError {

  final case class UserIdDoesNotExist(id: UserId) extends ConnectionError
  final case class UserIsAlreadyAConnection(current: UserId, other: UserId) extends ConnectionError
  final case class ConnectionRequestAlreadyMade(current: UserId, other: UserId) extends ConnectionError
  final case class NoConnectionRequest(current: UserId, other: UserId) extends ConnectionError

}
