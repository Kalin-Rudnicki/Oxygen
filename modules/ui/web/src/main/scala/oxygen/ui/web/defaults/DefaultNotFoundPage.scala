package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object DefaultNotFoundPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type Params = PageURL

  override def title(state: PageURL): String = "404 Not Found"

  override def postLoad(state: WidgetState[State]): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: PageURL): WidgetES[Any, PageURL] =
    fragment(
      PageMessagesBottomCorner.attached,
      h1("404 Not Found"),
      p(s"url: ${state.formatted}"),
    )

}
