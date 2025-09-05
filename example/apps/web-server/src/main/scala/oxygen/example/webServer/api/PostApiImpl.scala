package oxygen.example.webServer.api

import oxygen.example.api.*
import oxygen.example.api.model.error.*
import oxygen.example.api.model.post.*
import oxygen.example.api.model.user.*
import oxygen.example.api.service.TokenService
import oxygen.example.conversion.apiToDomain.*
import oxygen.example.conversion.domainToApi.*
import oxygen.example.core.model.post.PostId
import oxygen.example.core.model.user.UserId
import oxygen.example.domain.model.error.DomainError
import oxygen.example.domain.service.*
import oxygen.http.server.CurrentRequest
import zio.*

final case class PostApiImpl(
    tokenService: TokenService,
    connectionService: ConnectionService,
    postService: PostService,
) extends PostApi {

  override def createPost(authorization: UserToken, req: CreatePost): IO[ApiError, Post] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        post <- postService.createPost(user, req.toDomain)
      } yield post.toApi
    }

  override def createComment(postId: PostId, authorization: UserToken, req: CreateComment): IO[ApiError, Comment] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        comment <- postService.createComment(user, req.toDomain(postId))
      } yield comment.toApi
    }

  override def getPosts(userId: UserId, authorization: UserToken): IO[ApiError, Seq[Post]] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        posts <- postService.getPosts(user, userId)
      } yield posts.map(_.toApi)
    }

  override def getPost(postId: PostId, authorization: UserToken): IO[ApiError, Post] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        post <- postService.getPost(user, postId)
      } yield post.toApi
    }

  override def getComments(postId: PostId, authorization: UserToken): IO[ApiError, Seq[Comment]] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        comments <- postService.getComments(user, postId)
      } yield comments.map(_.toApi)
    }

}
object PostApiImpl {

  val layer: URLayer[TokenService & ConnectionService & PostService, PostApi] =
    ZLayer.fromFunction { PostApiImpl.apply }

}
