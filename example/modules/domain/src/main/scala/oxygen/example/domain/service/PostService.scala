package oxygen.example.domain.service

import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.post.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.*
import zio.*

final class PostService(
    postRepo: PostRepo,
    connectionService: ConnectionService,
) {

  def createPost(user: SimpleUser, req: CreatePost): IO[DomainError, Post] =
    for {
      now <- Clock.instant
      postId <- Random.nextUUID.map(PostId(_))
      post = Post(
        id = postId,
        userId = user.id,
        title = req.title,
        body = req.body,
        createdAt = now,
      )
      _ <- postRepo.insertPost(post)
    } yield post

  def createComment(user: SimpleUser, req: CreateComment): IO[DomainError, Comment] =
    for {
      post <- postRepo.getPost(req.postId)
      _ <- connectionService.ensureAccess(user, post.userId)
      now <- Clock.instant
      commentId <- Random.nextUUID.map(CommentId(_))
      comment = Comment(
        id = commentId,
        postId = post.id,
        userId = user.id,
        comment = req.comment,
        createdAt = now,
      )
      _ <- postRepo.insertComment(comment)
    } yield comment

  def getPosts(user: SimpleUser, userId: UserId): IO[DomainError, Seq[Post]] =
    for {
      _ <- connectionService.ensureAccess(user, userId)
      posts <- postRepo.postsByUser(userId)
    } yield posts

  def getPost(user: SimpleUser, postId: PostId): IO[DomainError, Post] =
    for {
      post <- postRepo.getPost(postId)
      _ <- connectionService.ensureAccess(user, post.userId)
    } yield post

  def getComments(user: SimpleUser, postId: PostId): IO[DomainError, Seq[Comment]] =
    for {
      post <- postRepo.getPost(postId)
      _ <- connectionService.ensureAccess(user, post.userId)
      comments <- postRepo.commentsForPost(postId)
    } yield comments

}
object PostService {

  val layer: URLayer[PostRepo & ConnectionService, PostService] =
    ZLayer.fromFunction { PostService.apply }

}
