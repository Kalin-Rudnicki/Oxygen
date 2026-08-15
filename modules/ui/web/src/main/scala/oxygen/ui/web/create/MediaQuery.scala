package oxygen.ui.web.create

import oxygen.ui.web.style.Breakpoints

/**
  * Composable media-query condition for the stylesheet DSL (OXY-158).
  *
  * Instead of dropping down to raw `@media (...)` CSS strings, build responsive
  * conditions with the same ergonomic, composable DSL as the rest of the sheet and
  * apply them with the [[oxygen.ui.web.create.media]] block:
  *
  * {{{
  * media(MediaQuery.belowMd) {
  *   MyClass(display.none)
  * }
  *
  * media(MediaQuery.mdUp && MediaQuery.landscape) {
  *   MyClass(flexDirection.row)
  * }
  * }}}
  *
  * `query` renders the raw condition text that follows `@media ` (e.g. `(min-width: 768px)`).
  */
sealed trait MediaQuery {

  /** The raw condition text placed after `@media ` (e.g. `(min-width: 768px) and (orientation: landscape)`). */
  def query: String

  /** Logical AND — both conditions must hold (`a and b`). */
  final def &&(that: MediaQuery): MediaQuery = MediaQuery.And(this, that)

  /** Logical AND — both conditions must hold (`a and b`). */
  final def and(that: MediaQuery): MediaQuery = MediaQuery.And(this, that)

  /** Logical OR — either condition may hold (comma list `a, b`). */
  final def ||(that: MediaQuery): MediaQuery = MediaQuery.Or(this, that)

  /** Logical OR — either condition may hold (comma list `a, b`). */
  final def or(that: MediaQuery): MediaQuery = MediaQuery.Or(this, that)

}
object MediaQuery {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Tree
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** A single, already-rendered condition, e.g. `(min-width: 768px)` or a media type like `screen`. */
  final case class Feature(rendered: String) extends MediaQuery {
    override def query: String = rendered
  }

  final case class And(a: MediaQuery, b: MediaQuery) extends MediaQuery {
    override def query: String = s"${a.query} and ${b.query}"
  }

  final case class Or(a: MediaQuery, b: MediaQuery) extends MediaQuery {
    override def query: String = s"${a.query}, ${b.query}"
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Constructors
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Escape hatch for any raw condition text (placed after `@media `). */
  def raw(rendered: String): MediaQuery = Feature(rendered)

  // ---  Width / height  ---

  def minWidth(px: Int): MediaQuery = Feature(s"(min-width: ${px}px)")
  def maxWidth(px: Int): MediaQuery = Feature(s"(max-width: ${px}px)")
  def minHeight(px: Int): MediaQuery = Feature(s"(min-height: ${px}px)")
  def maxHeight(px: Int): MediaQuery = Feature(s"(max-height: ${px}px)")

  // ---  Breakpoints (mobile-first, from Breakpoints scale)  ---

  /** `>= sm` (≥480px). */
  def smUp: MediaQuery = minWidth(Breakpoints.sm)

  /** `>= md` (≥768px). */
  def mdUp: MediaQuery = minWidth(Breakpoints.md)

  /** `>= lg` (≥1024px). */
  def lgUp: MediaQuery = minWidth(Breakpoints.lg)

  /** `>= xl` (≥1280px). */
  def xlUp: MediaQuery = minWidth(Breakpoints.xl)

  /** `< sm` — below the `sm` breakpoint. */
  def belowSm: MediaQuery = maxWidth(Breakpoints.sm - 1)

  /** `< md` — the typical "mobile shell" band. */
  def belowMd: MediaQuery = maxWidth(Breakpoints.md - 1)

  /** `< lg`. */
  def belowLg: MediaQuery = maxWidth(Breakpoints.lg - 1)

  /** `< xl`. */
  def belowXl: MediaQuery = maxWidth(Breakpoints.xl - 1)

  /** Inclusive-min / exclusive-max band `[minPx, maxPxExclusive)`. */
  def between(minPx: Int, maxPxExclusive: Int): MediaQuery =
    minWidth(minPx) && maxWidth(maxPxExclusive - 1)

  // ---  Color scheme  ---

  /** `(prefers-color-scheme: dark)`. */
  val prefersDark: MediaQuery = Feature("(prefers-color-scheme: dark)")

  /** `(prefers-color-scheme: light)`. */
  val prefersLight: MediaQuery = Feature("(prefers-color-scheme: light)")

  // ---  Orientation  ---

  val portrait: MediaQuery = Feature("(orientation: portrait)")
  val landscape: MediaQuery = Feature("(orientation: landscape)")

  // ---  Motion / interaction  ---

  /** `(prefers-reduced-motion: reduce)`. */
  val reducedMotion: MediaQuery = Feature("(prefers-reduced-motion: reduce)")

  /** `(prefers-reduced-motion: no-preference)`. */
  val allowsMotion: MediaQuery = Feature("(prefers-reduced-motion: no-preference)")

  /** `(hover: hover)` — pointing device that can hover (mouse, not touch). */
  val canHover: MediaQuery = Feature("(hover: hover)")

  /** `(pointer: coarse)` — imprecise pointer (touch). */
  val coarsePointer: MediaQuery = Feature("(pointer: coarse)")

  /** `(pointer: fine)` — precise pointer (mouse/stylus). */
  val finePointer: MediaQuery = Feature("(pointer: fine)")

  // ---  Media types  ---

  val screen: MediaQuery = Feature("screen")
  val print: MediaQuery = Feature("print")
  val all: MediaQuery = Feature("all")

}
