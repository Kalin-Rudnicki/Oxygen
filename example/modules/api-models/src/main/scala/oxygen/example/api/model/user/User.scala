package oxygen.example.api.model.user

import java.time.Instant
import oxygen.example.core.model.user.{*, given}
import oxygen.schema.JsonSchema

final case class User(
    id: UserId,
    email: Email,
    firstName: String,
    lastName: String,
    createdAt: Instant,
) derives JsonSchema {
  lazy val fullName: String = s"$firstName $lastName"
}
