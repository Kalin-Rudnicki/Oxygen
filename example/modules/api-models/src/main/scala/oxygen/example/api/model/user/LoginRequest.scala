package oxygen.example.api.model.user

import oxygen.example.core.model.user.{*, given}
import oxygen.schema.JsonSchema

final case class LoginRequest(
    email: Email,
    password: String,
) derives JsonSchema
