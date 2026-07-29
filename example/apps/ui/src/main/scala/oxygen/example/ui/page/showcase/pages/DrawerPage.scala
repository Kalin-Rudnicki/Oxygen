package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object DrawerPage extends RoutablePage.NoParams[Any] {
  final case class PageState(drawer: Drawer.State = Drawer.State())

  override val path: Seq[String] = Seq("showcase", "overlays", "drawer")
  override def title(s: PageState): String = "Drawer form"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(DrawerPage, "Drawer form")(
        Button("Open drawer").content(onClick := state.update(s => s.copy(drawer = s.drawer.show))),
        Drawer()(
          h3("Edit item"),
          div(
            Label("Name"),
            div(height := S.spacing._1),
            input(
              `type`.text,
              padding := S.spacing._2,
              width := 28.ch,
              border(1.px, "solid", S.color.fg.subtle),
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerTwo,
              color := S.color.fg.default,
            ),
          ),
          div(height := S.spacing._3),
          Button("Save").small,
        ).zoomOut[PageState](_.drawer),
      )

}
