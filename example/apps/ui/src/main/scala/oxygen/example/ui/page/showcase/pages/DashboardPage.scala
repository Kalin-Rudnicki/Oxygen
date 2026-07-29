package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object DashboardPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "dashboard")
  override def pageTitle: String = "Dashboard"

  private def metricCard(icon: Icon, title: String, value: String, accent: String): Widget =
    Section.level2(
      div(
        display.flex,
        alignItems.center,
        gap := S.spacing._3,
        // colored glyph tile
        span(
          display.inlineFlex,
          alignItems.center,
          justifyContent.center,
          width := 40.px,
          height := 40.px,
          borderRadius := S.borderRadius._3,
          backgroundColor := S.color.bg.layerTwo,
          color := accent,
          icon.lg,
        ),
        div(
          h3(title, margin := "0", fontSize := S.fontSize._3, color := S.color.fg.moderate),
          p(value, margin := "0", fontSize := S.fontSize._6, fontWeight := "700", color := S.color.fg.default),
        ),
      ),
    )

  override def body: Widget =
    fragment(
      ShowcaseLayout.todoBackend("Metrics / activity API"),
      ShowcaseLayout.note("Cards + table snippet + colored icons (currentColor)."),
      Row(
        Col.span(12).md(4)(metricCard(Icon.users, "Users", "1,284", S.color.primary.standard)).widget,
        Col.span(12).md(4)(metricCard(Icon.dollar, "Revenue", "$48k", S.color.status.positive.standard)).widget,
        Col.span(12).md(4)(metricCard(Icon.warning, "Errors", "3", S.color.status.alert.standard)).widget,
      ).widget,
      div(height := S.spacing._4),
      Section.level1(
        h3("Recent activity"),
        Table.basic(
          Table.header("When", "Event", "User"),
          Table.body(
            Table.row("2m", "Login", "ada@example.com"),
            Table.row("1h", "Invoice paid", "bob@example.com"),
            Table.row("3h", "Invite sent", "cara@example.com"),
          ),
        ),
      ),
    )
}

/**
  * Theme picker: 5 first-party Oxygen packs (no CZR). Light/dark is separate.
  * Selection is persisted (`oxygen.theme-pack`) and applied live via CSS var overrides.
  */
