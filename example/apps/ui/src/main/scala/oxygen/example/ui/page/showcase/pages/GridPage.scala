package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object GridPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "responsive", "grid")
  override def pageTitle: String = "Responsive grid"

  private def cell(label: String, accent: String = S.color.bg.layerTwo): Widget =
    div(
      backgroundColor := accent,
      padding := S.spacing._3,
      borderRadius := S.borderRadius._3,
      border(1.px, "solid", S.color.bg.layerThree),
      minHeight := 48.px,
      display.flex,
      alignItems.center,
      justifyContent.center,
      textAlign.center,
      fontSize := S.fontSize._2,
      fontWeight := "600",
      color := S.color.fg.default,
      label,
    )

  override def body: Widget =
    fragment(
      ShowcaseLayout.note(
        "12-column progressive grid (Row / Col). Mobile-first: xs base, then sm/md/lg/xl overrides. " +
          "Resize the browser (or narrow the window) and watch each demo reflow.",
      ),
      // 1. Equal cards
      h3("1 · Equal cards (xs12 → md6 → lg3)", marginBottom := S.spacing._3),
      p(color := S.color.fg.moderate, fontSize := S.fontSize._2, marginBottom := S.spacing._3, "Stack on phone, 2-up on tablet, 4-up on desktop."),
      Row(
        Col.span(12).md(6).lg(3)(cell("A · xs12 md6 lg3", S.color.primary.subtle)).widget,
        Col.span(12).md(6).lg(3)(cell("B · xs12 md6 lg3", S.color.primary.subtle)).widget,
        Col.span(12).md(6).lg(3)(cell("C · xs12 md6 lg3", S.color.primary.subtle)).widget,
        Col.span(12).md(6).lg(3)(cell("D · xs12 md6 lg3", S.color.primary.subtle)).widget,
      ).widget,
      div(height := S.spacing._6),
      // 2. Sidebar layout
      h3("2 · Sidebar + main (xs12 → md3 + md9)", marginBottom := S.spacing._3),
      p(color := S.color.fg.moderate, fontSize := S.fontSize._2, marginBottom := S.spacing._3, "Nav stacks above content on small screens; side-by-side from md up."),
      Row(
        Col.span(12).md(3)(cell("Sidebar", S.color.bg.layerThree)).widget,
        Col.span(12).md(9)(cell("Main content · fluid", S.color.bg.layerTwo)).widget,
      ).widget,
      div(height := S.spacing._6),
      // 3. Asymmetric dashboard
      h3("3 · Asymmetric dashboard (hero + tiles)", marginBottom := S.spacing._3),
      Row(
        Col.span(12).lg(8)(cell("Hero metric · lg8", S.color.status.informational.subtle)).widget,
        Col.span(12).md(6).lg(4)(cell("Tile · md6 lg4", S.color.status.positive.subtle)).widget,
        Col.span(12).md(6).lg(4)(cell("Tile · md6 lg4", S.color.status.alert.subtle)).widget,
        Col.span(12).md(6).lg(4)(cell("Tile · md6 lg4", S.color.status.negative.subtle)).widget,
        Col.span(12).md(6).lg(4)(cell("Tile · md6 lg4", S.color.primary.subtle)).widget,
      ).widget,
      div(height := S.spacing._6),
      // 4. Dense form columns
      h3("4 · Form columns (xs12 → sm6 → lg4)", marginBottom := S.spacing._3),
      p(color := S.color.fg.moderate, fontSize := S.fontSize._2, marginBottom := S.spacing._3, "Three fields that collapse cleanly without horizontal scroll."),
      Row(
        Col.span(12).sm(6).lg(4)(cell("First name")).widget,
        Col.span(12).sm(6).lg(4)(cell("Last name")).widget,
        Col.span(12).sm(12).lg(4)(cell("Email")).widget,
      ).widget,
      div(height := S.spacing._6),
      // 5. Breakpoint ladder
      h3("5 · Breakpoint ladder (one cell, many spans)", marginBottom := S.spacing._3),
      p(
        color := S.color.fg.moderate,
        fontSize := S.fontSize._2,
        marginBottom := S.spacing._3,
        "Single column whose span changes at every tier — watch the colored bar grow/shrink.",
      ),
      Row(
        Col.span(6).sm(7).md(8).lg(9).xl(10)(
          cell("xs6 → sm7 → md8 → lg9 → xl10", S.color.highlight.accent.subtle),
        ).widget,
        Col.span(6).sm(5).md(4).lg(3).xl(2)(
          cell("fills rest", S.color.bg.layerThree),
        ).widget,
      ).widget,
    )
}
