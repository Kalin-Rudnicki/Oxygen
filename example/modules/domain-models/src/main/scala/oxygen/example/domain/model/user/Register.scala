package oxygen.example.domain.model.user

import oxygen.example.core.model.user.Email

final case class Register(
    email: Email,
    firstName: String,
    lastName: String,
    password: String,
)
