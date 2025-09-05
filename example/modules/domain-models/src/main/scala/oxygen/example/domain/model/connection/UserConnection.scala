package oxygen.example.domain.model.connection

import java.time.Instant
import oxygen.example.domain.model.user.SimpleUser

final case class UserConnection(
    current: SimpleUser,
    other: SimpleUser,
    createdAt: Instant,
) {
  def isSelf: Boolean = current.id == other.id
}
