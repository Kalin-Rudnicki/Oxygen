package oxygen.example.domain.model.post

import java.time.Instant
import oxygen.example.core.model.*

final case class Comment(
    id: CommentId,
    postId: PostId,
    userId: UserId,
    comment: String,
    createdAt: Instant,
)
