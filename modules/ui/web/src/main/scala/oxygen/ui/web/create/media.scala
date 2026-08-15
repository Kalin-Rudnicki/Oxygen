package oxygen.ui.web.create

import oxygen.ui.web.internal.StyleSheetElement

/**
  * Apply a [[MediaQuery]] to every rule emitted inside `body` (OXY-158).
  *
  * This makes responsive styles first-class in the stylesheet DSL: instead of dropping down to raw
  * `@media (...)` CSS strings, wrap ordinary selector rules in a `media(...) { ... }` block and they
  * are emitted inside a correct `@media` block at compile time.
  *
  * {{{
  * object MySheet extends StyleSheetBuilder {
  *   object Card extends Class("card") {
  *     selector(display.flex, flexDirection.row)
  *
  *     // Stack on narrow viewports — same DSL, no raw strings.
  *     media(MediaQuery.belowMd) {
  *       selector(flexDirection.column)
  *     }
  *   }
  *
  *   override val compiled: StyleSheet = StyleSheet.derived[MySheet.type]
  * }
  * }}}
  *
  * Blocks nest and compose: a `media` inside another `media` ANDs the two conditions together.
  * The implicit [[StyleSheetBuilder.MutableAdder]] comes from the enclosing builder / class body,
  * exactly like a bare `selector(...)` call.
  */
def media(query: MediaQuery)(body: StyleSheetBuilder.MutableAdder ?=> Unit)(using outer: StyleSheetBuilder.MutableAdder): Unit = {
  val tagging: StyleSheetBuilder.MutableAdder =
    (ass: StyleSheetElement.AppliedStyleSheet) => outer.add(ass.withMedia(query))
  body(using tagging)
}
