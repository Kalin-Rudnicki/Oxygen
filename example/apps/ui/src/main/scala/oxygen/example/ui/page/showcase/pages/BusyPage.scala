package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object BusyPage extends RoutablePage.NoParams[Any] {
  final case class PageState(
      loading: Boolean = false,
      progress: Int = 0,
      contentReady: Boolean = false,
  )

  override val path: Seq[String] = Seq("showcase", "feedback", "busy")
  override def title(s: PageState): String = "Busy feedback"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  private def runSpinnerDemo(state: WidgetState[PageState]): UIO[Unit] =
    for {
      _ <- state.update(_.copy(loading = true))
      _ <- ZIO.sleep(1800.millis)
      _ <- state.update(_.copy(loading = false))
      _ <- PageMessages.add(PageMessage.positive("Load finished"))
    } yield ()

  private def runProgressDemo(state: WidgetState[PageState]): UIO[Unit] =
    for {
      _ <- state.update(_.copy(progress = 0))
      _ <- ZIO.foreachDiscard(1 to 20) { i =>
        ZIO.sleep(80.millis) *> state.update(_.copy(progress = i * 5))
      }
      _ <- PageMessages.add(PageMessage.info("Progress reached 100%"))
    } yield ()

  private def runSkeletonDemo(state: WidgetState[PageState]): UIO[Unit] =
    for {
      _ <- state.update(_.copy(contentReady = false))
      _ <- ZIO.sleep(1400.millis)
      _ <- state.update(_.copy(contentReady = true))
    } yield ()

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(BusyPage, "Busy feedback")(
        ShowcaseLayout.note(
          "Three busy patterns with real motion: spinning loader, determinate progress, pulsing skeleton. " +
            "Use them while async work runs — click each demo button.",
        ),
        // Spinner
        Section.level2(
          h3("Spinner"),
          p(color := S.color.fg.moderate, fontSize := S.fontSize._2, "Indeterminate wait (API call, auth). Glyph rotates via CSS."),
          div(height := S.spacing._3),
          if renderState.loading then Spinner(28, "Fetching profile…")
          else Button("Simulate 1.8s load").small.content(onClick := runSpinnerDemo(state)),
        ),
        div(height := S.spacing._6),
        // Progress
        Section.level2(
          h3("Progress bar"),
          p(color := S.color.fg.moderate, fontSize := S.fontSize._2, "Determinate work (upload, multi-step job)."),
          div(height := S.spacing._3),
          Progress.percent(renderState.progress),
          div(height := S.spacing._2, fontSize := S.fontSize._2, color := S.color.fg.subtle, s"${renderState.progress}%"),
          Button("Run to 100%").small.subtle.content(onClick := runProgressDemo(state)),
        ),
        div(height := S.spacing._6),
        // Skeleton
        Section.level2(
          h3("Skeleton"),
          p(color := S.color.fg.moderate, fontSize := S.fontSize._2, "Placeholder chrome while first paint of a panel loads."),
          div(height := S.spacing._3),
          if renderState.contentReady then
            div(
              padding := S.spacing._4,
              backgroundColor := S.color.bg.layerTwo,
              borderRadius := S.borderRadius._3,
              h4(margin := "0", "Dashboard snapshot"),
              p(marginTop := S.spacing._2, color := S.color.fg.default, "1,284 users · $48k revenue · 3 open errors"),
            )
          else Skeleton.lines(4),
          div(height := S.spacing._3),
          Button(if renderState.contentReady then "Reload with skeleton" else "Load content")
            .small
            .subtle
            .content(onClick := runSkeletonDemo(state)),
        ),
      )
}
