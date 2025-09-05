package oxygen.example.api.model.post

import java.time.Instant
import oxygen.example.core.model.post.{*, given}
import oxygen.example.core.model.user.{*, given}
import oxygen.schema.JsonSchema

final case class Post(
    id: PostId,
    userId: UserId,
    title: String,
    body: String,
    createdAt: Instant,
) derives JsonSchema
