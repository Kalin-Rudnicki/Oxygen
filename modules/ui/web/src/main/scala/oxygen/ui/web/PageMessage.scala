package oxygen.ui.web

import java.util.UUID
import oxygen.predef.core.*

final case class PageMessage(
    id: UUID,
    `type`: PageMessage.Type,
    content: Widget.Const,
)
object PageMessage {

  enum Type {
    case Success
    case Info
    case Warning
    case Error
    // case Custom(name: String)
  }

  def make(`type`: Type, content: Widget.Const): PageMessage = PageMessage(PlatformCompat.randomUUID(), `type`, content)

  def success(content: String): PageMessage = PageMessage.make(Type.Success, Widget.text(content))
  def info(content: String): PageMessage = PageMessage.make(Type.Info, Widget.text(content))
  def warning(content: String): PageMessage = PageMessage.make(Type.Warning, Widget.text(content))
  def error(content: String): PageMessage = PageMessage.make(Type.Error, Widget.text(content))
  // def custom(customName: String, content: String): PageMessage = PageMessage.make(Type.Custom(customName), Widget.text(content))

}
