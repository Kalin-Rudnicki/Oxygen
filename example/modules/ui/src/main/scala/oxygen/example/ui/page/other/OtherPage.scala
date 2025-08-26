package oxygen.example.ui.page.other

import oxygen.ui.web.{*, given}
import oxygen.ui.web.component.{*, given}
import oxygen.ui.web.defaults.PageErrorsBottomCorner
import zio.*

object OtherPage extends RoutablePage[Any] {

  final case class Params(
      initialCount: Int,
  )

  override lazy val paramCodec: PageCodec[Params] =
    ("page" / "other" / "initial-count" / PageCodec.path.plain[Int]).transform(Params.apply, _.initialCount)

  final case class State(
      initialCount: Int,
      newCount: Int,
  )

  override def paramsFromState(state: State): Params =
    Params(state.initialCount)

  override def initialLoad(params: Params): ZIO[Scope, UIError, State] =
    ZIO.fail(UIError.ExternalError("oopsie oopsieoopsie oopsie oopsie oopsie oopsieoopsie oopsie oopsie oopsie oopsieoopsie oopsie oopsie ")) *>
      ZIO.succeed(State(params.initialCount, params.initialCount))

  override def postLoad(state: State, rh: RaiseHandler.Stateful[Nothing, State]): ZIO[Scope, UIError, Unit] =
    // Clock.sleep(1.second) *>
    ZIO.fail(UIError.ExternalError("oopsie")) *>
      ZIO.sleep(7.seconds) *> (ZIO.sleep(500.millis) *> rh.updateState(s => s.copy(newCount = s.newCount + 1))).replicateZIODiscard(10)

  override def title(state: State): String = "Other"

  override protected def component(state: State): Widget.Stateful[Any, Nothing, State] =
    div(
      PageErrorsBottomCorner.lifted,
      h1("Other"),
      verticalSpacing,
      div(
        p(s"initial-count: ${state.initialCount}"),
        p(s"new-count: ${state.newCount}"),
      ),
      verticalSpacing,
      button(
        "Log Stuff",
        onClick := ZIO.logInfo("Hello Console"),
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val verticalSpacing: Widget.Const =
    div(height := "10px")

}
