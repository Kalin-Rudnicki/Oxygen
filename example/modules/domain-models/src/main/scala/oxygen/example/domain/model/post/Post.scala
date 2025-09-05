package oxygen.example.domain.model.post

import java.time.Instant
import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*

final case class Post(
    id: PostId,
    userId: UserId,
    title: String,
    body: String,
    createdAt: Instant,
)
