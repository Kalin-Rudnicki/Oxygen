package oxygen.example.domain.model.user

import oxygen.example.core.model.user.Email

final case class Login(
    email: Email,
    password: String,
)
