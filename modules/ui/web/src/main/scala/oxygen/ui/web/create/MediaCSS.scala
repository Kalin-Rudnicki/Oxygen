package oxygen.ui.web.create

import oxygen.ui.web.style.Breakpoints

/**
  * W5-T02: sanctioned helpers to emit `@media` CSS without a full media-query DSL.
  *
  * ''Prefer the composable [[MediaQuery]] + [[media]] DSL (OXY-158) for new stylesheet code'' —
  * it lets responsive rules be written with the same selector DSL as everything else and emits the
  * `@media` wrapper for you. These raw-string helpers remain for legacy raw sheets
  * (e.g. [[oxygen.ui.web.component.ColumnsStyle]], [[oxygen.ui.web.layout.HolyGrail]]) that are not
  * built through a [[StyleSheetBuilder]].
  */
object MediaCSS {

  def block(query: String)(body: String): String = {
    val indented = body.linesIterator.map { line =>
      if line.isEmpty then line else s"  $line"
    }.mkString("\n")
    s"@media $query {\n$indented\n}"
  }

  def minWidth(px: Int)(body: String): String =
    block(Breakpoints.minWidthPx(px))(body)

  def maxWidth(px: Int)(body: String): String =
    block(Breakpoints.maxWidthPx(px))(body)

  def smUp(body: String): String = minWidth(Breakpoints.sm)(body)
  def mdUp(body: String): String = minWidth(Breakpoints.md)(body)
  def lgUp(body: String): String = minWidth(Breakpoints.lg)(body)
  def xlUp(body: String): String = minWidth(Breakpoints.xl)(body)
  def belowMd(body: String): String = maxWidth(Breakpoints.md - 1)(body)

  /** Combine several media blocks into one StyleSheet. */
  def styleSheet(header: String)(blocks: String*): StyleSheet =
    StyleSheet.makeConst(header)(blocks.filter(_.nonEmpty).mkString("\n\n"))

}
