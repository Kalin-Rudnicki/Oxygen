package oxygen.example.domain.model.user

import oxygen.core.model.Email
import oxygen.crypto.model.Password

final case class Login(
    email: Email,
    password: Password.PlainText,
)
