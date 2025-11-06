package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object DefaultInitialPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type PageParams = Unit

  override def title(state: Unit): String = "Oxygen Web UI"

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    fragment(
      PageMessagesBottomCorner.attached,
      h1("Oxygen Web UI"),
    )

}
