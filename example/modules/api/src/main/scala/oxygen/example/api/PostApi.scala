package oxygen.example.api

import oxygen.example.api.model.error.*
import oxygen.example.api.model.post.*
import oxygen.example.api.model.user.*
import oxygen.example.core.model.post.{*, given}
import oxygen.example.core.model.user.{UserId, given}
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import zio.*

trait PostApi derives DeriveClient {

  @route.post("/api/post")
  def createPost(
      @param.header authorization: UserToken,
      @param.body req: CreatePost,
  ): IO[ApiError, Post]

  @route.post("/api/post/%/comment")
  def createComment(
      @param.path postId: PostId,
      @param.header authorization: UserToken,
      @param.body req: CreateComment,
  ): IO[ApiError, Comment]

  @route.get("/api/user/%/post")
  def getPosts(
      @param.path userId: UserId,
      @param.header authorization: UserToken,
  ): IO[ApiError, Seq[Post]]

  @route.get("/api/post/%")
  def getPost(
      @param.path postId: PostId,
      @param.header authorization: UserToken,
  ): IO[ApiError, Post]

  @route.get("/api/post/%/comment")
  def getComments(
      @param.path postId: PostId,
      @param.header authorization: UserToken,
  ): IO[ApiError, Seq[Comment]]

}
