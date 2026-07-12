package oxygen.example.api.model.user

import java.time.Instant
import oxygen.core.model.Email
import oxygen.example.core.model.*
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
