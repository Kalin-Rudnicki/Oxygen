package oxygen.example.api.model.user

import oxygen.crypto.model.Password
import oxygen.example.core.model.user.{*, given}
import oxygen.schema.JsonSchema

final case class LoginRequest(
    email: Email,
    password: Password.PlainText,
) derives JsonSchema
