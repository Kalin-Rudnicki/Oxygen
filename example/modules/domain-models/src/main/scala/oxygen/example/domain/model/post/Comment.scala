package oxygen.example.domain.model.post

import java.time.Instant
import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*

final case class Comment(
    id: CommentId,
    postId: PostId,
    userId: UserId,
    comment: String,
    createdAt: Instant,
)
