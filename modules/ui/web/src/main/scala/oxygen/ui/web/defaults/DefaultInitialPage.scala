package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.{*, given}
import zio.*

object DefaultInitialPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type Params = Unit

  override def title(state: Unit): String = "Oxygen Web UI"

  override def postLoad(state: Unit, rh: RaiseHandler.Stateful[Nothing, Unit]): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: State): Widget.Stateful[Any, Nothing, State] =
    fragment(
      PageErrorsBottomCorner.lifted,
      h1("Oxygen Web UI"),
    )

}
