package oxygen.example.domain.repo

import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.example.domain.model.error.DomainError
import oxygen.example.domain.model.post.*
import zio.*

trait PostRepo {

  def insertPost(post: Post): UIO[Unit]
  def findPost(postId: PostId): UIO[Option[Post]]
  def postsByUser(userId: UserId): UIO[Seq[Post]]

  def insertComment(comment: Comment): UIO[Unit]
  def commentsForPost(postId: PostId): UIO[Seq[Comment]]

  final def getPost(postId: PostId): IO[DomainError.PostIdDoesNotExist, Post] =
    findPost(postId).someOrFail(DomainError.PostIdDoesNotExist(postId))

}
