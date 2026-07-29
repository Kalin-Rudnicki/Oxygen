package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object SortablePage extends RoutablePage.NoParams[Any] {
  final case class PageState(
      onto: SortableList.State[String] = SortableList.State.of(Seq("Design", "Build", "Review", "Ship")),
      between: SortableList.State[String] =
        SortableList.State.of(Seq("Inbox", "Triage", "Doing", "Review", "Done")),
  )

  override val path: Seq[String] = Seq("showcase", "data", "sortable")
  override def title(s: PageState): String = "Sortable list"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(SortablePage, "Sortable list")(
        ShowcaseLayout.todoBackend("Persist order"),
        ShowcaseLayout.note(
          "Cursors: grab on idle rows, grabbing while dragging. " +
            "Each row is Widget.Stateful over A (zoom into items(i)). Tap + to mutate an element. " +
            "Drag chrome sheet via coreOxygenStyleSheets.",
        ),
        h3("Drop onto (highlight target row)", marginBottom := S.spacing._2),
        p(
          color := S.color.fg.moderate,
          fontSize := S.fontSize._2,
          marginBottom := S.spacing._3,
          "Drop on a row to place the item at that index.",
        ),
        SortableList(row).onto
          .rowBg(S.color.bg.layerOne)
          .rowRadius(S.borderRadius._3)
          .zoomOut[PageState](_.onto),
        div(height := S.spacing._6),
        h3("Drop between (horizontal insertion line)", marginBottom := S.spacing._2),
        p(
          color := S.color.fg.moderate,
          fontSize := S.fontSize._2,
          marginBottom := S.spacing._3,
          "Gaps light up with a primary bar; drop inserts at that slot.",
        ),
        SortableList(row).between
          .betweenSlotHeight(10)
          .handleColor(S.color.primary.standard)
          .zoomOut[PageState](_.between),
      )

  /** Row body: stateful over the element string. */
  private def row: WidgetS[String] =
    Widget.state[String].fixGet { (ws, name) =>
      fragment(
        Button("+").extraSmall.minimal.content(
          onClick := ws.update(_ + "+"),
        ),
        Spacing.horizontal._3,
        span(
          Widget.raw.css("flex", "1 1 auto"),
          minWidth := 0.px,
          fontWeight := "500",
          name,
        ),
      )
    }

}
