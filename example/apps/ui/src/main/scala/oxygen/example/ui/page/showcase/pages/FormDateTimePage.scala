package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object FormDateTimePage extends RoutablePage.NoParams[Any] {
  final case class PageState(
      date: DatePicker.State = DatePicker.State.today,
      time24: TimePicker.State = TimePicker.State.noon,
      time12: TimePicker.State = TimePicker.State.of(java.time.LocalTime.of(15, 30)),
      dt24: DateTimePicker.State = DateTimePicker.State.empty,
      dt12: DateTimePicker.State = DateTimePicker.State.now,
  )

  override val path: Seq[String] = Seq("showcase", "forms", "datetime")
  override def title(s: PageState): String = "Date / time pickers"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(FormDateTimePage, "Date / time pickers")(
        ShowcaseLayout.note(
          "Custom pickers (not native). Date header is a fixed CSS grid (nav cells never steal title space). " +
            "Time: 24h or 12h+AM/PM; type digits or use ▲/▼ (minutes step by 5).",
        ),
        h3("Date"),
        DatePicker.empty.zoomOut[PageState](_.date),
        div(height := S.spacing._5),
        h3("Time · 24-hour"),
        p(color := S.color.fg.moderate, fontSize := S.fontSize._2, marginBottom := S.spacing._2, "Hours 00–23."),
        TimePicker.h24.zoomOut[PageState](_.time24),
        div(height := S.spacing._4),
        h3("Time · 12-hour"),
        p(color := S.color.fg.moderate, fontSize := S.fontSize._2, marginBottom := S.spacing._2, "Hours 1–12 + AM/PM toggle (state is still 0–23)."),
        TimePicker.h12.zoomOut[PageState](_.time12),
        div(height := S.spacing._5),
        h3("DateTime · 24-hour"),
        p(color := S.color.fg.moderate, fontSize := S.fontSize._2, marginBottom := S.spacing._2, "Date panel + compact time (time no longer stretches to date width)."),
        DateTimePicker.h24.zoomOut[PageState](_.dt24),
        div(height := S.spacing._4),
        h3("DateTime · 12-hour"),
        DateTimePicker.h12.zoomOut[PageState](_.dt12),
      )
}
