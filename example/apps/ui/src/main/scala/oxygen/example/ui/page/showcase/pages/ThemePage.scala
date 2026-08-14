package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * OXY-154: this page is now a thin consumer of the reusable [[ColorModePicker]] / [[ThemePicker]]
  * helpers — the hand-rolled mode toggle + pack-card grid were extracted into those widgets.
  */
object ThemePage extends RoutablePage.NoParams[Any] {

  override type PageState = Unit

  override val path: Seq[String] = Seq("showcase", "theme")
  override def title(s: Unit): String = "Theme studio"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, Unit] = ZIO.unit
  override def postLoad(state: WidgetState[Unit], initialState: Unit): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[Unit], renderState: Unit): WidgetS[Unit] =
    ShowcaseLayout
      .page(ThemePage, "Theme studio")(
        ShowcaseLayout.note(
          "Themes: zinc Graphite core + colorways (Emerald / Rose / Amber / Indigo / Fuchsia), " +
            "plus surface personalities (Aurora / Ember / Violet / Ocean). " +
            "Top bar = solid primary + on-primary ink. Light/Dark flips surfaces only.",
        ),
        // mode — reusable picker, shown in its several variants
        h3("Color mode picker", marginBottom := S.spacing._3),
        p(
          color := S.color.fg.moderate,
          fontSize := S.fontSize._2,
          marginBottom := S.spacing._4,
          "Standalone Light/Dark/System control (independent of theme packs). " +
            "Keyboard: arrows / Home / End move + select; Space/Enter select. " +
            "All instances share one persisted preference.",
        ),
        div(
          display.flex,
          flexDirection.column,
          gap := S.spacing._4,
          marginBottom := S.spacing._5,
          // default segmented, with a leading caption
          ColorModePicker(label = Some("Segmented")),
          // segmented with icons
          ColorModePicker.segmentedWithIcons.label("With icons"),
          // Light/Dark only (no System)
          ColorModePicker().lightDarkOnly.label("Light / Dark only"),
          // compact icon cycle button (top-bar style)
          ColorModePicker.compact.label("Compact"),
        ),
        h3("Oxygen theme packs", marginBottom := S.spacing._3),
        p(
          color := S.color.fg.moderate,
          fontSize := S.fontSize._2,
          marginBottom := S.spacing._4,
          "Pick a pack — applied + persisted live. Tell us which pack becomes the framework default.",
        ),
        // pack picker — reusable widget renders OxygenThemes.all
        ThemePicker(),
      )
}

/** Full icon catalog + color samples. */
