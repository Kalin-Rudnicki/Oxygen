package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.Window
import zio.*

object AnchorsPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "docs", "anchors")
  override def pageTitle: String = "Hash anchors"

  private def go(id: String): UIO[Unit] =
    // set hash → Router hashchange → HashScroll; also works on hard refresh
    Window.location.setHash(id)

  override def body: Widget =
    fragment(
      ShowcaseLayout.note(
        "Section.withId + URL hashes. Buttons write `#intro` / `#api` / `#faq` so a refresh lands on the same section. " +
          "PageManager already runs HashScroll after first paint.",
      ),
      div(
        display.flex,
        gap := S.spacing._3,
        flexWrap.wrap,
        Button("→ Intro").small.subtle.content(onClick := go("intro")),
        Button("→ API").small.subtle.content(onClick := go("api")),
        Button("→ FAQ").small.subtle.content(onClick := go("faq")),
        Button("Clear hash").small.minimal.content(onClick := Window.location.setHash("")),
      ),
      div(height := 40.vh),
      Section.level1.withId("intro")(h2("Intro"), p("Anchor target intro. " * 20)),
      div(height := 40.vh),
      Section.level1.withId("api")(h2("API"), p("Anchor target api. " * 20)),
      div(height := 40.vh),
      Section.level1.withId("faq")(h2("FAQ"), p("Anchor target faq. " * 20)),
      div(height := 40.vh),
    )
}
