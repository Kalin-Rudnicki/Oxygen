package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object DefaultInitialPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type Params = Unit

  override def title(state: Unit): String = "Oxygen Web UI"

  override protected def component(state: State): WidgetS[State] =
    fragment(
      PageMessagesBottomCorner.attached,
      h1("Oxygen Web UI"),
    )

}
