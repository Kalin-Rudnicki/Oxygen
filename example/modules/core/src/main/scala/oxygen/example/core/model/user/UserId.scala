package oxygen.example.core.model.user

import java.util.UUID

final case class UserId(id: UUID) {
  override def toString: String = id.toString
}
