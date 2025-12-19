package oxygen.example.domain.repo

import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.example.domain.model.error.DomainError
import oxygen.example.domain.model.post.*
import oxygen.storage.CRUDRepo
import zio.*

trait PostRepo {

  val post: Posts
  val comment: Comments

  trait Posts extends CRUDRepo[PostId, Post] {

    def postsByUser(userId: UserId): UIO[Seq[Post]]

    final def getPost(postId: PostId): IO[DomainError.PostIdDoesNotExist, Post] =
      findByKey(postId).someOrFail(DomainError.PostIdDoesNotExist(postId))

  }

  trait Comments extends CRUDRepo[CommentId, Comment] {

    def commentsForPost(postId: PostId): UIO[Seq[Comment]]

  }

}
