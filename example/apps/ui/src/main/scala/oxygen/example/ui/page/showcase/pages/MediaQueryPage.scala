package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Responsive styles built entirely with the composable media-query DSL (OXY-158).
  *
  * Every responsive rule on this page is authored with the ordinary selector DSL wrapped in a
  * `media(MediaQuery.*) { ... }` block — no hand-written `@media` strings. The stylesheet compiles
  * to correct `@media (...) { selector { ... } }` CSS.
  */
object MediaQueryStyle extends StyleSheetBuilder {

  // Panel of cards: stacked on phones, row from `md` up.
  object Panel extends Class("mq-panel") { p =>

    Panel(
      display.flex,
      flexDirection.column,
      gap := S.spacing._3,
      padding := S.spacing._4,
      borderRadius := S.borderRadius._3,
      border(1.px, "solid", S.color.bg.layerThree),
      backgroundColor := S.color.bg.layerTwo,
    )

    // `md` and up → lay the cards out in a row. Same DSL as above; the wrapper is emitted for us.
    media(MediaQuery.mdUp) {
      Panel(flexDirection.row)
    }

    // Compose conditions: nudge the accent when the user prefers a dark color scheme.
    media(MediaQuery.prefersDark) {
      Panel(borderColor := S.color.primary.strong)
    }

    object Card extends p.Class("card") {
      Card(
        flex := "1 1 0",
        padding := S.spacing._4,
        borderRadius := S.borderRadius._2,
        backgroundColor := S.color.primary.subtle,
        color := S.color.fg.default,
        textAlign.center,
        fontWeight := "600",
      )
    }

  }

  // Responsive visibility helpers — one shows only below `md`, the other only from `md` up.
  object OnlyMobile extends Class("mq-only-mobile") {
    selector(display.none)
    media(MediaQuery.belowMd) { selector(display.block) }
  }

  object OnlyDesktop extends Class("mq-only-desktop") {
    selector(display.none)
    media(MediaQuery.mdUp) { selector(display.block) }
  }

  override val compiled: StyleSheet = StyleSheet.derived[MediaQueryStyle.type]

}

object MediaQueryPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "responsive", "media-query")
  override def pageTitle: String = "Media queries (DSL)"

  private def card(label: String): Widget =
    div(MediaQueryStyle.Panel.Card, label)

  override def body: Widget =
    fragment(
      ShowcaseLayout.note(
        "Responsive styles authored with the composable media-query DSL (OXY-158): " +
          "`media(MediaQuery.mdUp) { Panel(flexDirection.row) }` — no raw @media strings. " +
          "Resize the window across the md breakpoint (768px) to watch each demo react.",
      ),
      h3("1 · Reflow (stack on phone, row from md up)", marginBottom := S.spacing._3),
      p(
        color := S.color.fg.moderate,
        fontSize := S.fontSize._2,
        marginBottom := S.spacing._3,
        "The panel is `flex-direction: column` by default and switches to `row` inside `@media (min-width: 768px)`.",
      ),
      div(
        MediaQueryStyle.Panel,
        card("Card A"),
        card("Card B"),
        card("Card C"),
      ),
      div(height := S.spacing._6),
      h3("2 · Responsive visibility", marginBottom := S.spacing._3),
      p(
        color := S.color.fg.moderate,
        fontSize := S.fontSize._2,
        marginBottom := S.spacing._3,
        "Two classes whose `display` flips at the breakpoint — exactly one is visible at a time.",
      ),
      div(
        MediaQueryStyle.OnlyMobile,
        padding := S.spacing._3,
        borderRadius := S.borderRadius._2,
        backgroundColor := S.color.status.alert.subtle,
        color := S.color.fg.default,
        fontWeight := "600",
        "Narrow viewport — you are below md (< 768px).",
      ),
      div(
        MediaQueryStyle.OnlyDesktop,
        padding := S.spacing._3,
        borderRadius := S.borderRadius._2,
        backgroundColor := S.color.status.positive.subtle,
        color := S.color.fg.default,
        fontWeight := "600",
        "Wide viewport — you are at md or above (>= 768px).",
      ),
    )
}
