package oxygen.example.conversion

import oxygen.crypto.model.Password
import oxygen.example.db.model as Db
import oxygen.example.domain.model as Domain
import oxygen.transform.*

object dbToDomain {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[String, Password.Hashed] = Password.Hashed.unsafeWrapPasswordHash(_)
  given Transform[Db.UserRow, Domain.user.FullUser] = Transform.derived
  given Transform[Db.ConnectionRow, Domain.connection.Connection] =
    self =>
      Domain.connection.Connection(
        current = self.currentUserId,
        other = self.otherUserId,
        createdAt = self.createdAt,
      )
  given Transform[Db.ConnectionRequestRow, Domain.connection.ConnectionRequest] =
    self =>
      Domain.connection.ConnectionRequest(
        current = self.currentUserId,
        other = self.otherUserId,
        createdAt = self.createdAt,
      )

  extension (self: Db.UserRow) def toDomain: Domain.user.FullUser = self.transformInto
  extension (self: Db.ConnectionRow) def toDomain: Domain.connection.Connection = self.transformInto
  extension (self: Db.ConnectionRequestRow) def toDomain: Domain.connection.ConnectionRequest = self.transformInto

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Post
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[Db.PostRow, Domain.post.Post] = Transform.derived
  given Transform[Db.CommentRow, Domain.post.Comment] = Transform.derived

  extension (self: Db.PostRow) def toDomain: Domain.post.Post = self.transformInto
  extension (self: Db.CommentRow) def toDomain: Domain.post.Comment = self.transformInto

}
