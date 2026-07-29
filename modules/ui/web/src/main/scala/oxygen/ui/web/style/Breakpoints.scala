package oxygen.ui.web.style

/**
  * W5-T01: shared breakpoint scale (px min-widths).
  * Use with [[oxygen.ui.web.create.MediaCSS]] helpers to emit `@media` blocks.
  */
object Breakpoints {

  /** 0px — phones default (mobile first). */
  val xs: Int = 0

  /** ≥480px — large phones / small tablets. */
  val sm: Int = 480

  /** ≥768px — tablets. */
  val md: Int = 768

  /** ≥1024px — laptops. */
  val lg: Int = 1024

  /** ≥1280px — desktops. */
  val xl: Int = 1280

  val all: List[(String, Int)] =
    List("xs" -> xs, "sm" -> sm, "md" -> md, "lg" -> lg, "xl" -> xl)

  def minWidthPx(px: Int): String = s"(min-width: ${px}px)"
  def maxWidthPx(px: Int): String = s"(max-width: ${px}px)"

  def smUp: String = minWidthPx(sm)
  def mdUp: String = minWidthPx(md)
  def lgUp: String = minWidthPx(lg)
  def xlUp: String = minWidthPx(xl)

  /** Below `md` — typical “mobile shell” band. */
  def belowMd: String = maxWidthPx(md - 1)

}
