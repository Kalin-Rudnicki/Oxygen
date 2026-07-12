package oxygen.example.domain.model.user

import oxygen.core.model.Email
import oxygen.crypto.model.Password

final case class Register(
    email: Email,
    firstName: String,
    lastName: String,
    password: Password.PlainText,
)
