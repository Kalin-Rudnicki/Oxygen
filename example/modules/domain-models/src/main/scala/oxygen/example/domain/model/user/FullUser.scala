package oxygen.example.domain.model.user

import java.time.Instant
import oxygen.core.model.Email
import oxygen.crypto.model.Password
import oxygen.example.core.model.*
import oxygen.predef.core.*
import oxygen.stripe.model.StripeCustomerId

sealed trait FullUser {

  val id: UserId
  val email: Email
  val firstName: String
  val lastName: String
  val hashedPassword: Password.Hashed
  val optStripeCustomerId: Option[StripeCustomerId]
  val createdAt: Instant

  final lazy val fullName: String = s"$firstName $lastName"

  final lazy val show: String = s"User[id = $id, name = $fullName]"

  final def toSimple: SimpleUser =
    SimpleUser(
      id = this.id,
      email = this.email,
      firstName = this.firstName,
      lastName = this.lastName,
      createdAt = this.createdAt,
    )

  override final def toString: String = show

}
object FullUser {

  def apply(
      id: UserId,
      email: Email,
      firstName: String,
      lastName: String,
      hashedPassword: Password.Hashed,
      optStripeCustomerId: Option[StripeCustomerId],
      createdAt: Instant,
  ): FullUser =
    optStripeCustomerId match {
      case Some(stripeCustomerId) =>
        FullUser.WithStripe(
          id = id,
          email = email,
          firstName = firstName,
          lastName = lastName,
          hashedPassword = hashedPassword,
          stripeCustomerId = stripeCustomerId,
          createdAt = createdAt,
        )
      case None =>
        FullUser.WithoutStripe(
          id = id,
          email = email,
          firstName = firstName,
          lastName = lastName,
          hashedPassword = hashedPassword,
          createdAt = createdAt,
        )
    }

  final case class WithStripe(
      id: UserId,
      email: Email,
      firstName: String,
      lastName: String,
      hashedPassword: Password.Hashed,
      stripeCustomerId: StripeCustomerId,
      createdAt: Instant,
  ) extends FullUser {
    override val optStripeCustomerId: Option[StripeCustomerId] = stripeCustomerId.some
  }

  final case class WithoutStripe(
      id: UserId,
      email: Email,
      firstName: String,
      lastName: String,
      hashedPassword: Password.Hashed,
      createdAt: Instant,
  ) extends FullUser {

    override val optStripeCustomerId: Option[StripeCustomerId] = None

    def withStripeCustomerId(stripeCustomerId: StripeCustomerId): FullUser.WithStripe =
      FullUser.WithStripe(
        id = id,
        email = email,
        firstName = firstName,
        lastName = lastName,
        hashedPassword = hashedPassword,
        stripeCustomerId = stripeCustomerId,
        createdAt = createdAt,
      )

  }

}
