package oxygen.example.conversion

import oxygen.example.api.model as Api
import oxygen.example.core.model.post.*
import oxygen.example.domain.model as Domain
import oxygen.transform.*

object apiToDomain {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[Api.user.RegisterRequest, Domain.user.Register] = Transform.derived
  given Transform[Api.user.LoginRequest, Domain.user.Login] = Transform.derived
  given Transform[Api.user.User, Domain.user.SimpleUser] = Transform.derived

  extension (self: Api.user.RegisterRequest) def toDomain: Domain.user.Register = self.transformInto
  extension (self: Api.user.LoginRequest) def toDomain: Domain.user.Login = self.transformInto
  extension (self: Api.user.User) def toDomain: Domain.user.SimpleUser = self.transformInto

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Post
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[Api.post.CreatePost, Domain.post.CreatePost] = Transform.derived

  extension (self: Api.post.CreatePost) def toDomain: Domain.post.CreatePost = self.transformInto

  extension (self: Api.post.CreateComment)
    def toDomain(postId: PostId): Domain.post.CreateComment =
      Domain.post.CreateComment(
        postId = postId,
        comment = self.comment,
      )

}
