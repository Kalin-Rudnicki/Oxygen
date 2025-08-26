package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.{*, given}
import zio.*

object DefaultNotFoundPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type Params = PageURL

  override def title(state: PageURL): String = "404 Not Found"

  override def postLoad(state: PageURL, rh: RaiseHandler.Stateful[Nothing, PageURL]): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: State): Widget.Stateful[Any, Nothing, State] =
    fragment(
      PageErrorsBottomCorner.lifted,
      h1("Error"),
      p(state.formatted),
    )

}
