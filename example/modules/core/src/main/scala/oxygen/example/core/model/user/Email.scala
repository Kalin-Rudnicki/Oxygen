package oxygen.example.core.model.user

import oxygen.predef.base.*

final case class Email private (email: String) {

  def referenceEmail: Email = {
    val phase1 = email.toLowerCase
    val phase2 = phase1.replaceAll(Email.plusPart.regex, "@")
    val phase3 =
      (phase2.split('@').toList match
        case head :: tail => head.replaceAll("\\.", "") :: tail
        case Nil          => Nil
      ).mkString("@")
    Email(phase3)
  }

  override def toString: String = email

}
object Email {

  private val plusPart = "[+][^@]*@".r

  private val reg = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$".r

  def parseOption(email: String): Option[Email] =
    Option.when(reg.matches(email))(Email(email))

  def parseEither(email: String): Either[String, Email] =
    parseOption(email).toRight(s"malformed email '$email'")

  def unsafeParse(email: String): Email =
    parseEither(email) match
      case Right(value) => value
      case Left(error)  => throw new RuntimeException(error)

  def unsafeWrapWithoutValidation(email: String): Email =
    Email(email)

  given stringDecoder: StringDecoder[Email] =
    StringDecoder.string.mapOrFail(parseEither)

}
