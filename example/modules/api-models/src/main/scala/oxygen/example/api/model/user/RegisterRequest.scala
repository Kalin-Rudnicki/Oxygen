package oxygen.example.api.model.user

import oxygen.crypto.model.Password
import oxygen.example.core.model.user.{*, given}
import oxygen.schema.JsonSchema

final case class RegisterRequest(
    email: Email,
    firstName: String,
    lastName: String,
    password: Password.PlainText,
) derives JsonSchema
