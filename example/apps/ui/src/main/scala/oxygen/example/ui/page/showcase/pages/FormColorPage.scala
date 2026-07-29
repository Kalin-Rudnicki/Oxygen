package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.*
import zio.*

object FormColorPage extends RoutablePage.NoParams[Any] {
  final case class PageState(color: ColorPicker.State = ColorPicker.State.of("#3b82f6"))

  override val path: Seq[String] = Seq("showcase", "forms", "color")
  override def title(s: PageState): String = "Color field"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(FormColorPage, "Color field")(
        ShowcaseLayout.note("Hex normalize + presets + alpha."),
        ColorPicker.widget.zoomOut[PageState](_.color),
      )

}
