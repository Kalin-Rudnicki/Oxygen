package oxygen.example.api.model.user

import oxygen.example.core.model.user.{*, given}
import oxygen.schema.JsonSchema

final case class RegisterRequest(
    email: Email,
    firstName: String,
    lastName: String,
    password: String,
) derives JsonSchema
