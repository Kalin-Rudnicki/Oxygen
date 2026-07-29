package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.{ColorMode, Theme}
import oxygen.ui.web.style.OxygenThemes
import zio.*

object ThemePage extends RoutablePage.NoParams[Any] {
  final case class PageState(activeId: String = OxygenThemes.default.id)

  override val path: Seq[String] = Seq("showcase", "theme")
  override def title(s: PageState): String = "Theme studio"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] =
    Theme.currentId.map(PageState(_))
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(ThemePage, "Theme studio")(
        ShowcaseLayout.note(
          "Themes: zinc Graphite core + colorways (Emerald / Rose / Amber / Indigo / Fuchsia), " +
            "plus surface personalities (Aurora / Ember / Violet / Ocean). " +
            "Top bar = solid primary + on-primary ink. Light/Dark flips surfaces only.",
        ),
        // mode
        div(
          display.flex,
          alignItems.center,
          gap := S.spacing._2,
          flexWrap.wrap,
          marginBottom := S.spacing._5,
          span(color := S.color.fg.moderate, fontSize := S.fontSize._2, "Color mode"),
          Button("Light").small.subtle.content(onClick := ColorMode.setAndPersist(ColorMode.Mode.Light)),
          Button("Dark").small.subtle.content(onClick := ColorMode.setAndPersist(ColorMode.Mode.Dark)),
          Button("System").small.subtle.content(onClick := ColorMode.setAndPersist(ColorMode.Mode.System)),
        ),
        h3("Oxygen theme packs", marginBottom := S.spacing._3),
        p(
          color := S.color.fg.moderate,
          fontSize := S.fontSize._2,
          marginBottom := S.spacing._4,
          s"Active: ${OxygenThemes.byId.get(renderState.activeId).map(_.name).getOrElse(renderState.activeId)}. Tell us which pack becomes the framework default.",
        ),
        div(
          display.flex,
          flexDirection.column,
          gap := S.spacing._3,
          Widget.foreach(OxygenThemes.all.toList) { pack =>
            val on = pack.id == renderState.activeId
            div(
              display.flex,
              flexDirection.column,
              gap := S.spacing._3,
              padding := S.spacing._4,
              borderRadius := S.borderRadius._4,
              border(2.px, "solid", if on then S.color.primary.standard else S.color.bg.layerThree),
              backgroundColor := S.color.bg.layerOne,
              // header row
              div(
                display.flex,
                alignItems.center,
                justifyContent.spaceBetween,
                gap := S.spacing._3,
                flexWrap.wrap,
                div(
                  div(fontWeight := "700", fontSize := S.fontSize._5, color := S.color.fg.default, pack.name),
                  div(fontSize := S.fontSize._2, color := S.color.fg.moderate, marginTop := S.spacing._1, pack.blurb),
                ), {
                  val label = if on then "Selected" else "Use this theme"
                  val base = Button(label).small
                  val btn = if on then base.primary else base.subtle
                  btn.content(
                    onClick := (
                      Theme.applyAndPersist(pack) *>
                        state.update(_.copy(activeId = pack.id)) *>
                        PageMessages.schedule(PageMessage.positive(s"Theme: ${pack.name}"), 2.seconds)
                    ),
                  )
                },
              ),
              // swatches
              div(
                display.flex,
                gap := S.spacing._2,
                flexWrap.wrap,
                themeSwatch("Primary", pack.primarySwatch),
                themeSwatch("Accent", pack.accentSwatch),
                themeSwatch("BG", pack.bgSwatch),
                themeSwatch("Danger", pack.dark.danger),
                themeSwatch("Success", pack.dark.success),
              ),
              // live preview chips using current CSS vars when selected; raw hex chips always
              div(
                display.flex,
                gap := S.spacing._2,
                flexWrap.wrap,
                alignItems.center,
                Button("Primary").small.primary,
                Button("Accent").small, // default intent
                Button("Danger").small.negative.subtle,
                Button("Success").small.positive.subtle,
                span(color := S.color.fg.subtle, fontSize := S.fontSize._1, "← live tokens (active pack)"),
              ),
            )
          },
        ),
      )

  private def themeSwatch(label: String, hex: String): Widget =
    div(
      display.flex,
      flexDirection.column,
      alignItems.center,
      gap := S.spacing._1,
      div(
        width := 40.px,
        height := 40.px,
        borderRadius := S.borderRadius._3,
        backgroundColor := hex,
        border(1.px, "solid", S.color.bg.layerThree),
      ),
      span(fontSize := S.fontSize._1, color := S.color.fg.subtle, label),
      span(fontSize := S.fontSize._1, color := S.color.fg.minimal, hex),
    )
}

/** Full icon catalog + color samples. */
