package oxygen.example.domain.model.post

import oxygen.example.core.model.post.*

final case class CreateComment(
    postId: PostId,
    comment: String,
)
