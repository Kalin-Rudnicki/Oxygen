package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object KitchenSinkPage extends RoutablePage.NoParams[Any] {
  final case class PageState(
      modal: Option[Unit] = None,
      pager: Pagination.State = Pagination.State.initial(5, 12),
      color: ColorPicker.State = ColorPicker.State.of("#3b82f6"),
  )

  override val path: Seq[String] = Seq("showcase", "kitchen-sink")
  override def title(s: PageState): String = "Kitchen sink"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(KitchenSinkPage, "Kitchen sink")(
        ShowcaseLayout.todoBackend("Admin APIs for combined console"),
        ShowcaseLayout.note("Shell + form + table + modal + color in one path."),
        div(
          display.flex,
          gap := S.spacing._2,
          flexWrap.wrap,
          ColorModePicker(), // OXY-154: reusable picker
          Button("Open modal").small.content(onClick := state.update(_.copy(modal = Some(())))),
          Button("Toast").small.informational.content(onClick := PageMessages.add(PageMessage.info("Kitchen sink toast"))),
        ),
        div(height := S.spacing._4),
        div(
          Label("Search"),
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
        ColorPicker.widget.zoomOut[PageState](_.color),
        div(height := S.spacing._3),
        Pagination.controls.zoomOut[PageState](_.pager),
        Table.ofData("Id", "Label")(renderState.pager.slice(0 until 12)) { i =>
          Seq(Widget.text(i.toString), Widget.text(s"Item $i"))
        },
        Modal
          .option()(
            h2("Kitchen modal"),
            p("Combined smoke."),
            Button("Close").small.content(onClick.action(Modal.Close)),
          )
          .zoomOut[PageState](_.modal),
      )

}
