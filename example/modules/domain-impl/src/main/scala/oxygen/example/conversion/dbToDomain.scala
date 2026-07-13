package oxygen.example.conversion

import oxygen.crypto.model.Password
import oxygen.example.db.model as Db
import oxygen.example.domain.model as Domain
import oxygen.sql.model.TypedJsonb
import oxygen.transform.*

object dbToDomain {

  given [A] => Transform[TypedJsonb[A], A] = _.value

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[String, Password.Hashed] = Password.Hashed.unsafeWrapPasswordHash(_)
  given Transform[Db.UserRow, Domain.user.FullUser] = Transform.derived

  extension (self: Db.UserRow) def toDomain: Domain.user.FullUser = self.transformInto

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

  given Transform[Db.PostRow, Domain.post.Post] = Transform.derived
  given Transform[Db.CommentRow, Domain.post.Comment] = Transform.derived

  extension (self: Db.PostRow) def toDomain: Domain.post.Post = self.transformInto
  extension (self: Db.CommentRow) def toDomain: Domain.post.Comment = self.transformInto

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Payment
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[Db.InitPaymentMethodRow, Domain.payment.InitPaymentMethod] = Transform.derived
  given Transform[Db.PaymentMethodRow, Domain.payment.PaymentMethod] = Transform.derived

  extension (self: Db.InitPaymentMethodRow) def toDomain: Domain.payment.InitPaymentMethod = self.transformInto
  extension (self: Db.PaymentMethodRow) def toDomain: Domain.payment.PaymentMethod = self.transformInto

}
