package oxygen.example.conversion

import oxygen.example.db.model as Db
import oxygen.example.domain.model as Domain

object domainToDb {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Domain.user.FullUser)
    def toDb: Db.UserRow =
      Db.UserRow(
        userId = self.id,
        email = self.email,
        referenceEmail = self.email.referenceEmail,
        firstName = self.firstName,
        lastName = self.lastName,
        hashedPassword = self.hashedPassword.value,
        createdAt = self.createdAt,
      )

  extension (self: Domain.connection.ConnectionRequest)
    def toDb: Db.ConnectionRequestRow =
      Db.ConnectionRequestRow(
        currentUserId = self.current,
        otherUserId = self.other,
        createdAt = self.createdAt,
      )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Post
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Domain.post.Post)
    def toDb: Db.PostRow =
      Db.PostRow(
        id = self.id,
        userId = self.userId,
        title = self.title,
        body = self.body,
        createdAt = self.createdAt,
      )

  extension (self: Domain.post.Comment)
    def toDb: Db.CommentRow =
      Db.CommentRow(
        id = self.id,
        postId = self.postId,
        userId = self.userId,
        comment = self.comment,
        createdAt = self.createdAt,
      )

}
