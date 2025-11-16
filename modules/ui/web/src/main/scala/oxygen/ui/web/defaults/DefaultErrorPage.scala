package oxygen.ui.web.defaults

import oxygen.ui.web.{UIError, *}
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object DefaultErrorPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type PageParams = Cause[UIError.NonRedirect]

  override def title(state: Cause[UIError.NonRedirect]): String = "Page Load Error"

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    fragment(
      PageMessagesBottomCorner.default,
      h1("Error"),
      p(whiteSpace.pre, renderState.prettyPrint),
    )

}
