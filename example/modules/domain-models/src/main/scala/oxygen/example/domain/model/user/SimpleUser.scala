package oxygen.example.domain.model.user

import java.time.Instant
import oxygen.example.core.model.user.*

final case class SimpleUser(
    id: UserId,
    email: Email,
    firstName: String,
    lastName: String,
    createdAt: Instant,
) {
  lazy val fullName: String = s"$firstName $lastName"

  lazy val show: String = s"User[id = ${id.id}, name = $fullName]"

  override def toString: String = show

}
