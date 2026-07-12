package oxygen.example.domain.model.error

import oxygen.core.model.Email

sealed trait RegistrationError
object RegistrationError {
  final case class EmailAlreadyExists(email: Email) extends RegistrationError
}
