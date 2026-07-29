package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object ModalPage extends RoutablePage.NoParams[Any] {
  final case class PageState(
      openMd: Option[Unit] = None,
      openSm: Option[Unit] = None,
      openLg: Option[Unit] = None,
  )

  override val path: Seq[String] = Seq("showcase", "overlays", "modal")
  override def title(s: PageState): String = "Modal flows"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(ModalPage, "Modal flows")(
        ShowcaseLayout.note(
          "Modal decorator: size presets (sm/md/lg/xl/full), padding (compact/comfortable), " +
            "max-height scroll, scrim opacity, elevation. Click outside or Close to dismiss.",
        ),
        div(
          display.flex,
          gap := S.spacing._2,
          flexWrap.wrap,
          Button("Default (md)").content(onClick := state.update(_.copy(openMd = Some(())))),
          Button("Small confirm").small.subtle.content(onClick := state.update(_.copy(openSm = Some(())))),
          Button("Large form").small.subtle.content(onClick := state.update(_.copy(openLg = Some(())))),
        ),
        Modal
          .option()(
            h2("Confirm action", marginTop := 0.px),
            p("Default md size · comfortable padding · soft elevation."),
            div(height := S.spacing._3),
            Button("Close").small.content(onClick.action(Modal.Close)),
          )
          .zoomOut[PageState](_.openMd),
        Modal
          .option(_.sm)(
            h3("Delete item?", marginTop := 0.px),
            p("Compact sm dialog for confirms."),
            div(height := S.spacing._3),
            div(
              display.flex,
              gap := S.spacing._2,
              Button("Cancel").small.subtle.content(onClick.action(Modal.Close)),
              Button("Delete").small.negative.content(onClick.action(Modal.Close)),
            ),
          )
          .zoomOut[PageState](_.openSm),
        Modal
          .option(_.lg.comfortable)(
            h2("Edit profile", marginTop := 0.px),
            p("Larger surface for multi-field forms. Body scrolls if content exceeds max-height."),
            div(height := S.spacing._3),
            div(
              Label("Display name"),
              div(height := S.spacing._1),
              input(
                `type`.text,
                padding := S.spacing._2,
                width := 100.pct,
                maxWidth := 28.ch,
                border(1.px, "solid", S.color.fg.subtle),
                borderRadius := S.borderRadius._3,
                backgroundColor := S.color.bg.layerTwo,
                color := S.color.fg.default,
              ),
            ),
            div(height := S.spacing._3),
            div(
              Label("Bio"),
              div(height := S.spacing._1),
              textArea(
                Widget.raw.htmlAttr("rows", "4"),
                padding := S.spacing._2,
                width := 100.pct,
                minHeight := 6.rem,
                border(1.px, "solid", S.color.fg.subtle),
                borderRadius := S.borderRadius._3,
                backgroundColor := S.color.bg.layerTwo,
                color := S.color.fg.default,
              ),
            ),
            div(height := S.spacing._4),
            Button("Save").small.primary.content(onClick.action(Modal.Close)),
          )
          .zoomOut[PageState](_.openLg),
      )
}
