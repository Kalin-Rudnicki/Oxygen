package oxygen.example.conversion

import oxygen.core.model.currency.PreciseMoney
import oxygen.example.db.model as Db
import oxygen.example.domain.model as Domain
import oxygen.sql.model.TypedJsonb
import oxygen.transform.*

object domainToDb {

  given [A] => Transform[A, TypedJsonb[A]] = TypedJsonb(_)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      User
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Domain.user.FullUser)
    def toDb: Db.UserRow =
      Db.UserRow(
        id = self.id,
        email = self.email,
        referenceEmail = self.email.normalize,
        firstName = self.firstName,
        lastName = self.lastName,
        hashedPassword = self.hashedPassword.getPasswordHash,
        stripeCustomerId = self.optStripeCustomerId,
        createdAt = self.createdAt,
      )

  extension (self: Domain.connection.Connection)
    def toDb: Db.ConnectionRow =
      Db.ConnectionRow(
        currentUserId = self.current,
        otherUserId = self.other,
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

  given Transform[Domain.post.Post, Db.PostRow] = Transform.derived
  given Transform[Domain.post.Comment, Db.CommentRow] = Transform.derived

  extension (self: Domain.post.Post) def toDb: Db.PostRow = self.transformInto
  extension (self: Domain.post.Comment) def toDb: Db.CommentRow = self.transformInto

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Payment
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given Transform[PreciseMoney, Db.PreciseMoneyColumn] = value => Db.PreciseMoneyColumn(value.toBigDecimal, value.currencyCode)
  
  given Transform[Domain.payment.InitPaymentMethod, Db.InitPaymentMethodRow] = Transform.derived
  given Transform[Domain.payment.PaymentMethod, Db.PaymentMethodRow] = Transform.derived
  given Transform[Domain.payment.Payment, Db.PaymentRow] = Transform.derived

  extension (self: Domain.payment.InitPaymentMethod) def toDb: Db.InitPaymentMethodRow = self.transformInto
  extension (self: Domain.payment.PaymentMethod) def toDb: Db.PaymentMethodRow = self.transformInto
  extension (self: Domain.payment.Payment) def toDb: Db.PaymentRow = self.transformInto

}
