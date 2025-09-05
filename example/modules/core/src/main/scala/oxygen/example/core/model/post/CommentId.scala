package oxygen.example.core.model.post

import java.util.UUID

final case class CommentId(id: UUID) {
  override def toString: String = id.toString
}
