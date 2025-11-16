package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object DefaultNotFoundPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type PageParams = PageURL

  override def title(state: PageURL): String = "404 Not Found"

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[Any, PageURL] =
    fragment(
      PageMessagesBottomCorner.default,
      h1("404 Not Found"),
      p(s"url: ${renderState.formatted}"),
    )

}
