package oxygen.core.model

import oxygen.core.{str, Text}
import oxygen.core.error.Error
import oxygen.core.syntax.option.*
import oxygen.core.typeclass.{Showable, StringCodec}

final case class Email private (
    username: String,
    tag: Option[String],
    domain: String,
) extends Showable {

  def normalize: Email =
    Email(
      username = username.toLowerCase.replace(".", ""),
      tag = None,
      domain = domain.toLowerCase,
    )

  override def show: Text = str"${Text.fromString(username)}${tag.fold(Text.empty)(tag => str"+$tag")}@$domain"

}
object Email {

  private val regex = "^([A-Za-z0-9.\\-_%]+)(?:\\+([A-Za-z0-9.\\-_%+]+))?@((?:[A-Za-z0-9\\-]+\\.)+[A-Za-z]{2,})$".r

  def fromString(email: String): Option[Email] = email match
    case regex(username, tag, domain) => Email(username, Option(tag), domain).some
    case _                            => None

  def unsafe(email: String): Email = fromString(email).getOrElse { throw Error(s"Invalid email: $email") }

  given stringCodec: StringCodec[Email] = StringCodec.string.transformOption(Email.fromString, _.toString)

}
