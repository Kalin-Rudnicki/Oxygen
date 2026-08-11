package oxygen.core.model

import oxygen.core.{str, Text}
import oxygen.core.error.Error
import oxygen.core.syntax.option.*
import oxygen.core.typeclass.{Show, Showable, StringCodec}

final case class Email private (
    username: String,
    subaddress: Option[Email.Subaddress],
    domain: String,
) extends Showable {

  /** The subaddress tag (the portion after the separator), if present. */
  def tag: Option[String] = subaddress.map(_.tag)

  def normalize: Email = normalize(Email.StripDots.Auto)

  def normalize(stripDots: Email.StripDots): Email = {
    val loweredUsername = username.toLowerCase
    val loweredDomain = domain.toLowerCase
    val shouldStrip = stripDots match
      case Email.StripDots.Auto => Email.dotInsensitiveDomains.contains(loweredDomain)
      case Email.StripDots.Yes  => true
      case Email.StripDots.No   => false
    Email(
      username = if shouldStrip then loweredUsername.replace(".", "") else loweredUsername,
      subaddress = None,
      domain = loweredDomain,
    )
  }

  override def show: Text =
    str"${Text.fromString(username)}${subaddress.fold(Text.empty)(s => str"${s.separator.toString}${s.tag}")}@$domain"

}
object Email {

  /**
    * Controls whether dots in the username are stripped during [[Email.normalize]].
    *
    * Unlike plus-addressing (which the RFCs leave to the provider but is widely honored), dot-insensitivity is
    * NOT part of any standard. Gmail chose to ignore dots in the local part, but most providers -- Outlook,
    * Yahoo, ProtonMail, Fastmail, and essentially every custom domain -- treat dots as significant, so stripping
    * them there would collapse two genuinely different mailboxes.
    */
  enum StripDots {

    /** Strip dots only for domains known to ignore them (see [[Email.dotInsensitiveDomains]]). */
    case Auto

    /** Always strip dots from the username. */
    case Yes

    /** Never strip dots from the username. */
    case No

  }

  /** Domains known to treat the username as dot-insensitive. Must be lowercase. */
  private val dotInsensitiveDomains: Set[String] = Set("gmail.com", "googlemail.com")

  /**
    * Subaddressing (a.k.a. "tag" / "plus") addressing lets `username<sep>tag@domain` route to `username@domain`.
    * The separator is provider-specific and NOT standardized: most providers (Gmail, Outlook, iCloud, ProtonMail,
    * Fastmail) use `+`, while the Yahoo family uses `-`. Unknown/custom domains fall back to `+`, the de-facto
    * default. Only the domain's own separator is treated as a tag boundary, so a `-` in a Gmail username stays part
    * of the username.
    */
  private val DefaultTagSeparator: Char = '+'

  private val tagSeparators: Map[String, Char] =
    Map(
      "yahoo.com" -> '-',
      "ymail.com" -> '-',
      "rocketmail.com" -> '-',
    )

  /** The subaddressing separator used by `domain` (case-insensitive), defaulting to `+`. */
  def tagSeparatorFor(domain: String): Char = tagSeparators.getOrElse(domain.toLowerCase, DefaultTagSeparator)

  /** A parsed subaddress: the provider-specific `separator` paired with the `tag` that followed it. */
  final case class Subaddress(separator: Char, tag: String)

  private val localPartRegex = "^[A-Za-z0-9.\\-_%+]+$".r
  private val domainRegex = "^(?:[A-Za-z0-9\\-]+\\.)+[A-Za-z]{2,}$".r

  def fromString(email: String): Option[Email] =
    email.split("@", -1) match
      case Array(localPart, domain) if localPartRegex.matches(localPart) && domainRegex.matches(domain) =>
        val separator = tagSeparatorFor(domain)
        localPart.indexOf(separator.toInt) match
          case -1 =>
            Email(localPart, None, domain).some
          case idx =>
            val username = localPart.substring(0, idx)
            val tag = localPart.substring(idx + 1)
            // reject an empty username (leading separator) or empty tag (trailing separator)
            Option.when(username.nonEmpty && tag.nonEmpty)(Email(username, Some(Email.Subaddress(separator, tag)), domain))
      case _ =>
        None

  def unsafe(email: String): Email = fromString(email).getOrElse { throw Error(s"Invalid email: $email") }

  given stringCodec: StringCodec[Email] = StringCodec.string.transformOption(Email.fromString, _.toString)

  given show: Show[Email] = Show.usingToString

}
