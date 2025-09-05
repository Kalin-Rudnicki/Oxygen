package oxygen.ui.web.defaults

import oxygen.ui.web.{UIError, *}
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object DefaultErrorPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type Params = Cause[UIError.NonRedirect]

  override def title(state: Cause[UIError.NonRedirect]): String = "Page Load Error"

  override protected def component(state: State): WidgetS[State] =
    fragment(
      PageMessagesBottomCorner.attached,
      h1("Error"),
      p(whiteSpace.pre, state.prettyPrint),
    )

}
