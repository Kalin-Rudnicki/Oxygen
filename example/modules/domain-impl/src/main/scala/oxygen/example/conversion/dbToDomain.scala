package oxygen.example.conversion

import oxygen.example.db.model as Db
import oxygen.example.domain.model as Domain

object dbToDomain {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Db.UserRow)
    def toDomain: Domain.user.FullUser =
      Domain.user.FullUser(
        id = self.userId,
        email = self.email,
        firstName = self.firstName,
        lastName = self.lastName,
        hashedPassword = Domain.user.HashedPassword(self.hashedPassword),
        createdAt = self.createdAt,
      )

  extension (self: Db.ConnectionRow)
    def toDomain: Domain.connection.Connection =
      Domain.connection.Connection(
        current = self.currentUserId,
        other = self.otherUserId,
        createdAt = self.createdAt,
      )

  extension (self: Db.ConnectionRequestRow)
    def toDomain: Domain.connection.ConnectionRequest =
      Domain.connection.ConnectionRequest(
        current = self.currentUserId,
        other = self.otherUserId,
        createdAt = self.createdAt,
      )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Post
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Db.PostRow)
    def toDomain: Domain.post.Post =
      Domain.post.Post(
        id = self.id,
        userId = self.userId,
        title = self.title,
        body = self.body,
        createdAt = self.createdAt,
      )

  extension (self: Db.CommentRow)
    def toDomain: Domain.post.Comment =
      Domain.post.Comment(
        id = self.id,
        postId = self.postId,
        userId = self.userId,
        comment = self.comment,
        createdAt = self.createdAt,
      )

}
