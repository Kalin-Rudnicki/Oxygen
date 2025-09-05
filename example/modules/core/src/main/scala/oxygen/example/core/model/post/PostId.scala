package oxygen.example.core.model.post

import java.util.UUID

final case class PostId(id: UUID) {
  override def toString: String = id.toString
}
