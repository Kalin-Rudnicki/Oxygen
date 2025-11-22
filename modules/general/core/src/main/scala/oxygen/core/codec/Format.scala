package oxygen.core.codec

trait Format {
  def name: String
}
object Format {

  /**
    * Represents a plain-text string.
    */
  type PlainText = PlainText.type
  case object PlainText extends Format {
    override val name: String = "PlainText"
  }

  /**
    * Represents a value in code.
    */
  type Value = Value.type
  case object Value extends Format {
    override val name: String = "Value"
  }

  sealed trait Base64 extends Format
  object Base64 {

    sealed trait Standard extends Base64
    sealed trait Raw extends Standard
    sealed trait Url extends Standard
    sealed trait Mime extends Standard

    sealed trait Padding extends Base64
    sealed trait WithPadding extends Padding
    sealed trait WithoutPadding extends Padding

    type RawWithPadding = RawWithPadding.type
    case object RawWithPadding extends Raw, WithPadding {
      override def name: String = "Base64 (With Padding)"
    }

    type RawWithoutPadding = RawWithoutPadding.type
    case object RawWithoutPadding extends Raw, WithoutPadding {
      override def name: String = "Base64 (Without Padding)"
    }

    type UrlWithPadding = UrlWithPadding.type
    case object UrlWithPadding extends Url, WithPadding {
      override def name: String = "Base64 Url (With Padding)"
    }

    type UrlWithoutPadding = UrlWithoutPadding.type
    case object UrlWithoutPadding extends Url, WithoutPadding {
      override def name: String = "Base64 (Without Padding)"
    }

    type MimeWithPadding = MimeWithPadding.type
    case object MimeWithPadding extends Mime, WithPadding {
      override def name: String = "Base64 Mime (With Padding)"
    }

    type MimeWithoutPadding = MimeWithoutPadding.type
    case object MimeWithoutPadding extends Mime, WithoutPadding {
      override def name: String = "Base64 Mime (Without Padding)"
    }

  }

}
