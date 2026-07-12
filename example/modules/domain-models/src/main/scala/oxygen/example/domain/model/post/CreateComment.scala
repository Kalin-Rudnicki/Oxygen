package oxygen.example.domain.model.post

import oxygen.example.core.model.*

final case class CreateComment(
    postId: PostId,
    comment: String,
)
