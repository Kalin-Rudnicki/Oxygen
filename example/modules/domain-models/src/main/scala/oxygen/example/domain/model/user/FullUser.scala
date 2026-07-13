package oxygen.example.domain.model.user

import java.time.Instant
import oxygen.core.model.Email
import oxygen.crypto.model.Password
import oxygen.example.core.model.*
import oxygen.stripe.model.StripeCustomerId

final case class FullUser(
    id: UserId,
    email: Email,
    firstName: String,
    lastName: String,
    hashedPassword: Password.Hashed,
    stripeCustomerId: Option[StripeCustomerId],
    createdAt: Instant,
) {

  lazy val fullName: String = s"$firstName $lastName"

  lazy val show: String = s"User[id = $id, name = $fullName]"

  def toSimple: SimpleUser =
    SimpleUser(
      id = this.id,
      email = this.email,
      firstName = this.firstName,
      lastName = this.lastName,
      createdAt = this.createdAt,
    )

}
