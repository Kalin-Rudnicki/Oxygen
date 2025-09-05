package oxygen.example.domain.model.error

import oxygen.example.core.model.user.Email

sealed trait RegistrationError
object RegistrationError {
  final case class EmailAlreadyExists(email: Email) extends RegistrationError
}
