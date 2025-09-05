package oxygen.example.conversion

import oxygen.example.api.model as Api
import oxygen.example.core.model.post.*
import oxygen.example.domain.model as Domain

object apiToDomain {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Api.user.RegisterRequest)
    def toDomain: Domain.user.Register =
      Domain.user.Register(
        email = self.email,
        firstName = self.firstName,
        lastName = self.lastName,
        password = self.password,
      )

  extension (self: Api.user.LoginRequest)
    def toDomain: Domain.user.Login =
      Domain.user.Login(
        email = self.email,
        password = self.password,
      )

  extension (self: Api.user.User)
    def toDomain: Domain.user.SimpleUser =
      Domain.user.SimpleUser(
        id = self.id,
        email = self.email,
        firstName = self.firstName,
        lastName = self.lastName,
        createdAt = self.createdAt,
      )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Post
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Api.post.CreatePost)
    def toDomain: Domain.post.CreatePost =
      Domain.post.CreatePost(
        title = self.title,
        body = self.body,
      )

  extension (self: Api.post.CreateComment)
    def toDomain(postId: PostId): Domain.post.CreateComment =
      Domain.post.CreateComment(
        postId = postId,
        comment = self.comment,
      )

}
