package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.{*, given}
import zio.*

object DefaultErrorPage extends NonRoutablePage.StateSameAsParams[Any] {

  override type Params = Cause[UIError.NonRedirect]

  override def title(state: Cause[UIError.NonRedirect]): String = "Page Load Error"

  override def postLoad(state: Cause[UIError.NonRedirect], rh: RaiseHandler.Stateful[Nothing, Cause[UIError.NonRedirect]]): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: State): Widget.Stateful[Any, Nothing, State] =
    fragment(
      PageErrorsBottomCorner.lifted,
      h1("Error"),
      p(whiteSpace.pre, state.prettyPrint),
    )

}
