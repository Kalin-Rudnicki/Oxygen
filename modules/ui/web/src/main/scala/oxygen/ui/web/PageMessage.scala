package oxygen.ui.web

import java.util.UUID
import oxygen.predef.core.*
import oxygen.ui.web.create.{given, *}

final case class PageMessage(
    id: UUID,
    `type`: PageMessage.Type,
    content: Widget,
)
object PageMessage {

  final case class Styling(
      backgroundColor: String,
      fontColor: String,
      buttonColor: String,
      borderColor: String,
  )
  object Styling {

    def make(
        base: CSSColor,
    ): Styling =
      Styling(
        backgroundColor = base.lighten(50.0),
        fontColor = base.darken(66.0),
        buttonColor = base.darken(20.0),
        borderColor = base.darken(20.0),
      )

  }

  enum Type {
    case Primary
    case Positive
    case Negative
    case Info
    case Warning
    case Error
    case Custom(styling: Styling)
  }

  def make(`type`: Type, content: Widget): PageMessage = PageMessage(PlatformCompat.randomUUID(), `type`, content)

  def primary(content: String): PageMessage = PageMessage.make(Type.Primary, Widget.text(content))
  def positive(content: String): PageMessage = PageMessage.make(Type.Positive, Widget.text(content))
  def negative(content: String): PageMessage = PageMessage.make(Type.Negative, Widget.text(content))
  def info(content: String): PageMessage = PageMessage.make(Type.Info, Widget.text(content))
  def warning(content: String): PageMessage = PageMessage.make(Type.Warning, Widget.text(content))
  def error(content: String): PageMessage = PageMessage.make(Type.Error, Widget.text(content))
  def custom(colors: Styling, content: String): PageMessage = PageMessage.make(Type.Custom(colors), Widget.text(content))

  def primary(content: Widget*): PageMessage = PageMessage.make(Type.Primary, Widget.fragment(content))
  def positive(content: Widget*): PageMessage = PageMessage.make(Type.Positive, Widget.fragment(content))
  def negative(content: Widget*): PageMessage = PageMessage.make(Type.Negative, Widget.fragment(content))
  def info(content: Widget*): PageMessage = PageMessage.make(Type.Info, Widget.fragment(content))
  def warning(content: Widget*): PageMessage = PageMessage.make(Type.Warning, Widget.fragment(content))
  def error(content: Widget*): PageMessage = PageMessage.make(Type.Error, Widget.fragment(content))
  def custom(colors: Styling, content: Widget*): PageMessage = PageMessage.make(Type.Custom(colors), Widget.fragment(content))

}
