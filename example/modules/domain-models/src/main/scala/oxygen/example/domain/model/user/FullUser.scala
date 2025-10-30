package oxygen.example.domain.model.user

import java.time.Instant
import oxygen.crypto.model.Password
import oxygen.example.core.model.user.*

final case class FullUser(
    id: UserId,
    email: Email,
    firstName: String,
    lastName: String,
    hashedPassword: Password.Hashed,
    createdAt: Instant,
) {
  lazy val fullName: String = s"$firstName $lastName"
  def toSimple: SimpleUser =
    SimpleUser(
      id = this.id,
      email = this.email,
      firstName = this.firstName,
      lastName = this.lastName,
      createdAt = this.createdAt,
    )
}
