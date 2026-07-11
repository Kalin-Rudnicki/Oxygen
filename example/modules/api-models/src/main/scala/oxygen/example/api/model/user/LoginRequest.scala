package oxygen.example.api.model.user

import oxygen.core.model.Email
import oxygen.crypto.model.Password
import oxygen.schema.JsonSchema

final case class LoginRequest(
    email: Email,
    password: Password.PlainText,
) derives JsonSchema
