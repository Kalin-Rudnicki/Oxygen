package oxygen.example.domain.model.user

import oxygen.crypto.model.Password
import oxygen.example.core.model.user.Email

final case class Register(
    email: Email,
    firstName: String,
    lastName: String,
    password: Password.PlainText,
)
