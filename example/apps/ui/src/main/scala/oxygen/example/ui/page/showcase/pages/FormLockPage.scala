package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object FormLockPage extends RoutablePage.NoParams[Any] {
  final case class PageState(busy: Boolean = false)

  override val path: Seq[String] = Seq("showcase", "forms", "lock")
  override def title(s: PageState): String = "Page lock submit"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(FormLockPage, "Page lock submit")(
        ShowcaseLayout.todoBackend("Async save under real PageLock.withPageLock"),
        ShowcaseLayout.note("Mock busy flag disables button and shows spinner."),
        div(
          Label("Title"),
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
        if renderState.busy then Spinner.medium
        else
          Button("Save")
            .leading(Icon.save)
            .content(
              onClick := (
                state.update(_.copy(busy = true)) *>
                  ZIO.sleep(800.millis) *>
                  state.update(_.copy(busy = false)) *>
                  PageMessages.add(PageMessage.positive("Saved (mock)"))
              ),
            ),
      )

}
