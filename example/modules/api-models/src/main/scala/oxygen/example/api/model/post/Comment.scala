package oxygen.example.api.model.post

import java.time.Instant
import oxygen.example.core.model.post.{*, given}
import oxygen.example.core.model.user.{*, given}
import oxygen.schema.JsonSchema

final case class Comment(
    id: CommentId,
    postId: PostId,
    userId: UserId,
    comment: String,
    createdAt: Instant,
) derives JsonSchema
