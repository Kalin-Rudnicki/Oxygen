package oxygen.example.core.model

import java.util.UUID
import oxygen.predef.core.*

opaque type UserId <: UUIDNewType = UUIDNewType
object UserId {
  def apply(value: UUID): UserId = UUIDNewType(value)
}

opaque type PostId <: UUIDNewType = UUIDNewType
object PostId {
  def apply(value: UUID): PostId = UUIDNewType(value)
}

opaque type CommentId <: UUIDNewType = UUIDNewType
object CommentId {
  def apply(value: UUID): CommentId = UUIDNewType(value)
}
