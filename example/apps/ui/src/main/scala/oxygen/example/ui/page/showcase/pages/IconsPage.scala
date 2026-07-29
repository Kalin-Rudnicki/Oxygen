package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object IconsPage extends RoutablePage.NoParams[Any] {
  final case class PageState(filter: String = "")

  override val path: Seq[String] = Seq("showcase", "icons")
  override def title(s: PageState): String = "Icons"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] = {
    val q = renderState.filter.trim.toLowerCase
    val icons = Icon.all.filter(i => q.isEmpty || i.name.toLowerCase.contains(q)).toList
    val colorSamples: List[(String, String)] = List(
      "fg.default" -> S.color.fg.default,
      "fg.moderate" -> S.color.fg.moderate,
      "fg.subtle" -> S.color.fg.subtle,
      "primary" -> S.color.primary.standard,
      "accent" -> S.color.highlight.accent.standard,
      "positive" -> S.color.status.positive.standard,
      "negative" -> S.color.status.negative.standard,
      "alert" -> S.color.status.alert.standard,
      "info" -> S.color.status.informational.standard,
    )
    ShowcaseLayout
      .page(IconsPage, "Icons")(
        ShowcaseLayout.note(
          s"Built-in stroke set (${Icon.all.size} icons). Uses currentColor — wrap in a colored parent to recolor.",
        ),
        // color demo
        h3("Colors", marginBottom := S.spacing._3),
        div(
          display.flex,
          flexWrap.wrap,
          gap := S.spacing._4,
          marginBottom := S.spacing._6,
          Widget.foreach(colorSamples) { case (label, col) =>
            div(
              display.flex,
              flexDirection.column,
              alignItems.center,
              gap := S.spacing._2,
              padding := S.spacing._3,
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerOne,
              border(1.px, "solid", S.color.bg.layerThree),
              minWidth := 88.px,
              span(color := col, display.inlineFlex, gap := S.spacing._2, Icon.star.lg, Icon.heart.lg, Icon.check.lg),
              span(fontSize := S.fontSize._1, color := S.color.fg.subtle, label),
            )
          },
        ),
        // filter
        h3(s"Catalog (${icons.size} / ${Icon.all.size})", marginBottom := S.spacing._3),
        div(
          display.flex,
          alignItems.center,
          gap := S.spacing._3,
          marginBottom := S.spacing._4,
          span(color := S.color.fg.moderate, "Filter"),
          input(
            `type`.text,
            value := renderState.filter,
            onInput.e.handle { e =>
              val v = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value
              state.update(_.copy(filter = v))
            },
            width := 28.ch,
            padding := S.spacing._2,
            border(1.px, "solid", S.color.bg.layerThree),
            borderRadius := S.borderRadius._3,
            backgroundColor := S.color.bg.layerTwo,
            color := S.color.fg.default,
            Widget.raw.htmlAttr("placeholder", "search, settings, chevron…"),
          ),
        ),
        div(
          display.flex,
          flexWrap.wrap,
          gap := S.spacing._2,
          Widget.foreach(icons) { ic =>
            div(
              display.flex,
              flexDirection.column,
              alignItems.center,
              justifyContent.center,
              gap := S.spacing._2,
              width := 100.px,
              height := 88.px,
              padding := S.spacing._2,
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerOne,
              border(1.px, "solid", S.color.bg.layerThree),
              color := S.color.fg.default,
              userSelect.none,
              ic.md,
              span(
                fontSize := S.fontSize._1,
                color := S.color.fg.subtle,
                textAlign.center,
                O.Ellipsis,
                maxWidth := 92.px,
                userSelect.none,
                ic.name,
              ),
            )
          },
        ),
      )
  }
}
