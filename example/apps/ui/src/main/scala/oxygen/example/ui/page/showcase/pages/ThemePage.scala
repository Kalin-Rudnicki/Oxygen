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
        // mode — reusable picker
        div(
          marginBottom := S.spacing._5,
          ColorModePicker(label = Some("Color mode")),
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
