package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object TablePage extends RoutablePage.NoParams[Any] {
  final case class PageState(pager: Pagination.State = Pagination.State.initial(10, 57))

  override val path: Seq[String] = Seq("showcase", "data", "table")
  override def title(s: PageState): String = "Table + pagination"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  private val allRows: IndexedSeq[Int] = IndexedSeq.range(0, 57)
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] = {
    val rows = renderState.pager.slice(allRows)
    ShowcaseLayout
      .page(TablePage, "Table + pagination")(
        ShowcaseLayout.todoBackend("Server-side list API"),
        ShowcaseLayout.note(
          "Quiet Table defaults: soft header band, uppercase labels, roomy cells, row hover, subtle rules. " +
            "Use Table.branded for a solid primary header when you want more punch.",
        ),
        Pagination.controls.zoomOut[PageState](_.pager),
        div(height := S.spacing._3),
        Table.basic(
          thead(tr(th("#"), th("Name"), th("Token"), th("Status"))),
          tbody(
            Widget.foreach(rows.toList) { i =>
              val status = if i % 5 == 0 then "error" else if i % 3 == 0 then "pending" else "ok"
              tr(
                td(i.toString),
                td(s"Row $i"),
                td(
                  span(fontFamily := "ui-monospace, monospace", fontSize := S.fontSize._2, color := S.color.fg.moderate, f"tok-$i%04x"),
                ),
                td(
                  span(
                    fontSize := S.fontSize._1,
                    fontWeight := "600",
                    padding := css(S.spacing._1, S.spacing._2),
                    borderRadius := S.borderRadius._2,
                    backgroundColor := (
                      if status == "ok" then S.color.status.positive.subtle
                      else if status == "pending" then S.color.status.alert.subtle
                      else S.color.status.negative.subtle
                    ),
                    color := (
                      if status == "ok" then S.color.status.positive.standard
                      else if status == "pending" then S.color.status.alert.standard
                      else S.color.status.negative.standard
                    ),
                    status,
                  ),
                ),
              )
            },
          ),
        ),
      )
  }
}
