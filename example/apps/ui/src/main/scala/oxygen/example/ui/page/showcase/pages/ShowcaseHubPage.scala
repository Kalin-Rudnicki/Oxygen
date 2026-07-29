package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object ShowcaseHubPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase")
  override def pageTitle: String = "Showcase hub"
  override def body: Widget =
    fragment(
      ShowcaseLayout.note("Oxygen UI showcase — use the left nav. Theme studio has Graphite + colorways (and surface packs); Icons shows the full catalog."),
      Section.level1(
        div(padding := S.spacing._2, "• Shell, auth, dashboard, theme packs, icons"),
        div(padding := S.spacing._2, "• Forms: validation, lock, choices, datetime, color, upload, all-fields"),
        div(padding := S.spacing._2, "• Overlays: modal, drawer, tooltips"),
        div(padding := S.spacing._2, "• Data: table, feed, sortable"),
        div(padding := S.spacing._2, "• Chrome: tabs, wizard steps, busy, messages, anchors, grid"),
        div(padding := S.spacing._2, "• Kitchen sink — combined smoke"),
      ),
      div(height := S.spacing._4),
      Button("Open shell demo").primary.content(onClick.push(ShellPage.nav())),
      span(display.inlineBlock, width := S.spacing._3),
      Button("Kitchen sink").subtle.content(onClick.push(KitchenSinkPage.nav())),
    )
}
