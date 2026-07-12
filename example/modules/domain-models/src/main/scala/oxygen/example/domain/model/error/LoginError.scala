package oxygen.example.domain.model.error

import oxygen.core.model.Email

sealed trait LoginError
object LoginError {
  final case class EmailDoesNotExist(email: Email) extends LoginError
  final case class InvalidPassword(email: Email) extends LoginError
}
