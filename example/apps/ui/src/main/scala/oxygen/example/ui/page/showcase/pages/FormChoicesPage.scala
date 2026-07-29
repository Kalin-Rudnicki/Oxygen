package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object FormChoicesPage extends RoutablePage.NoParams[Any] {
  final case class PageState(a: Boolean = true, b: Boolean = false, t: Boolean = false)
  override val path: Seq[String] = Seq("showcase", "forms", "choices")
  override def title(s: PageState): String = "Choice controls"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout.page(FormChoicesPage, "Choice controls")(
      ShowcaseLayout.note("Checkbox + toggle with page state."),
      Checkbox("Enable notifications").boolean.zoomOut[PageState](_.a),
      div(height := S.spacing._3),
      Checkbox("Marketing emails").boolean.zoomOut[PageState](_.b),
      div(height := S.spacing._3),
      ToggleThumb.boolean.zoomOut[PageState](_.t),
      div(height := S.spacing._2),
      span("ToggleThumb", color := S.color.fg.subtle),
    )
}
