package oxygen.example.conversion

import oxygen.core.model.currency.PreciseMoney
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
  given Transform[Db.UserRow, Domain.user.FullUser] =
    self =>
      Domain.user.FullUser(
        id = self.id,
        email = self.email,
        firstName = self.firstName,
        lastName = self.lastName,
        hashedPassword = self.hashedPassword.transformInto,
        optStripeCustomerId = self.stripeCustomerId,
        createdAt = self.createdAt,
      )
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Payment
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[Db.PreciseMoneyColumn, PreciseMoney] = value => PreciseMoney(value.amount, value.currency)

  given Transform[Db.InitPaymentMethodRow, Domain.payment.InitPaymentMethod] = Transform.derived
  given Transform[Db.PaymentMethodRow, Domain.payment.PaymentMethod] = Transform.derived
  given Transform[Db.PaymentRow, Domain.payment.Payment] = Transform.derived

  extension (self: Db.InitPaymentMethodRow) def toDomain: Domain.payment.InitPaymentMethod = self.transformInto
  extension (self: Db.PaymentMethodRow) def toDomain: Domain.payment.PaymentMethod = self.transformInto
  extension (self: Db.PaymentRow) def toDomain: Domain.payment.Payment = self.transformInto

}
