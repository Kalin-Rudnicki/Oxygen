package oxygen.example.conversion

import oxygen.example.db.model as Db
import oxygen.example.domain.model as Domain
import oxygen.transform.*

object domainToDb {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[Domain.user.FullUser, Db.UserRow] =
    self =>
      Db.UserRow(
        id = self.id,
        email = self.email,
        referenceEmail = self.email.referenceEmail,
        firstName = self.firstName,
        lastName = self.lastName,
        hashedPassword = self.hashedPassword.getPasswordHash,
        createdAt = self.createdAt,
      )
  given Transform[Domain.connection.Connection, Db.ConnectionRow] =
    self =>
      Db.ConnectionRow(
        currentUserId = self.current,
        otherUserId = self.other,
        createdAt = self.createdAt,
      )
  given Transform[Domain.connection.ConnectionRequest, Db.ConnectionRequestRow] =
    self =>
      Db.ConnectionRequestRow(
        currentUserId = self.current,
        otherUserId = self.other,
        createdAt = self.createdAt,
      )

  extension (self: Domain.user.FullUser) def toDb: Db.UserRow = self.transformInto

  extension (self: Domain.connection.Connection) def toDb: Db.ConnectionRow = self.transformInto

  extension (self: Domain.connection.ConnectionRequest) def toDb: Db.ConnectionRequestRow = self.transformInto

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Post
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[Domain.post.Post, Db.PostRow] = Transform.derived
  given Transform[Domain.post.Comment, Db.CommentRow] = Transform.derived

  extension (self: Domain.post.Post) def toDb: Db.PostRow = self.transformInto
  extension (self: Domain.post.Comment) def toDb: Db.CommentRow = self.transformInto

}
