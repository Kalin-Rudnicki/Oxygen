package oxygen.example.conversion

import oxygen.example.api.model as Api
import oxygen.example.domain.model as Domain

object domainToApi {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Error
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Domain.error.RegistrationError)
    def toApi: Api.error.RegistrationError =
      self match {
        case Domain.error.RegistrationError.EmailAlreadyExists(email) => Api.error.RegistrationError.EmailAlreadyExists(email)
      }

  extension (self: Domain.error.LoginError)
    def toApi: Api.error.LoginError =
      self match {
        case Domain.error.LoginError.EmailDoesNotExist(_) => Api.error.LoginError.InvalidCredentials
        case Domain.error.LoginError.InvalidPassword(_)   => Api.error.LoginError.InvalidCredentials
      }

  extension (self: Domain.error.DomainError)
    def toApi: Api.error.ApiError =
      self match {
        case Domain.error.DomainError.UserIdDoesNotExist(id)     => Api.error.ApiError.NotFound.noUserMessage(s"No such user id $id")
        case Domain.error.DomainError.UserIsNotAConnection(_, _) => Api.error.ApiError.Unauthorized.userSafe("You are not connected with this user")
        case Domain.error.DomainError.PostIdDoesNotExist(id)     => Api.error.ApiError.NotFound.noUserMessage(s"No such post id $id")
      }

  extension (self: Domain.error.ConnectionError)
    def toApi: Api.error.ApiError =
      self match {
        case Domain.error.ConnectionError.UserIdDoesNotExist(id)             => Api.error.ApiError.NotFound.noUserMessage(s"No such user id $id")
        case Domain.error.ConnectionError.UserIsAlreadyAConnection(_, _)     => Api.error.ApiError.Conflict.userSafe("You are already connected with this user")
        case Domain.error.ConnectionError.ConnectionRequestAlreadyMade(_, _) => Api.error.ApiError.Conflict.userSafe("Connection request already sent")
        case Domain.error.ConnectionError.NoConnectionRequest(_, _)          => Api.error.ApiError.NotFound.noUserMessage("No connection request to decision")
      }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Domain.user.SimpleUser)
    def toApi: Api.user.User =
      Api.user.User(
        id = self.id,
        email = self.email,
        firstName = self.firstName,
        lastName = self.lastName,
        createdAt = self.createdAt,
      )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Post
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Domain.post.Post)
    def toApi: Api.post.Post =
      Api.post.Post(
        id = self.id,
        userId = self.userId,
        title = self.title,
        body = self.body,
        createdAt = self.createdAt,
      )

  extension (self: Domain.post.Comment)
    def toApi: Api.post.Comment =
      Api.post.Comment(
        id = self.id,
        postId = self.postId,
        userId = self.userId,
        comment = self.comment,
        createdAt = self.createdAt,
      )

}
