package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W5-T03: 12-column progressive grid (Row / Col).
  * Spans collapse via CSS classes + [[MediaCSS]] rules in [[ColumnsStyle]].
  */
final case class Row(
    private val gutter: String,
    private val children: Widget,
) {
  def gutter(g: String): Row = copy(gutter = g)
  def apply(more: Widget*): Row =
    copy(children = fragment(this.children, Widget.fragment(more)))

  def widget: Widget =
    div(
      Widget.`class`("oxy-row"),
      Widget.raw.css("gap", gutter),
      children,
    )
}
object Row {
  def apply(): Row = Row(S.spacing._4, Widget.empty)
  def apply(children: Widget*): Row = Row(S.spacing._4, Widget.fragment(children))
}

/**
  * Column span per breakpoint (1–12). Zero means “inherit smaller breakpoint”.
  * Mobile-first: `xs` is the base class; larger breakpoints override when set (>0).
  */
final case class Col(
    private val xs: Int,
    private val sm: Int,
    private val md: Int,
    private val lg: Int,
    private val xl: Int,
    private val children: Widget,
) {
  def xs(n: Int): Col = copy(xs = Col.clamp(n))
  def sm(n: Int): Col = copy(sm = Col.clamp(n))
  def md(n: Int): Col = copy(md = Col.clamp(n))
  def lg(n: Int): Col = copy(lg = Col.clamp(n))
  def xl(n: Int): Col = copy(xl = Col.clamp(n))

  def apply(more: Widget*): Col =
    copy(children = fragment(this.children, Widget.fragment(more)))

  def widget: Widget = {
    val classes: Seq[String] =
      List(
        Some(s"oxy-col-xs-$xs"),
        Option.when(sm > 0)(s"oxy-col-sm-$sm"),
        Option.when(md > 0)(s"oxy-col-md-$md"),
        Option.when(lg > 0)(s"oxy-col-lg-$lg"),
        Option.when(xl > 0)(s"oxy-col-xl-$xl"),
      ).flatten
    div(
      Widget.`class`(classes*),
      minWidth := 0,
      children,
    )
  }
}
object Col {
  private def clamp(n: Int): Int = math.max(0, math.min(12, n))

  /** Full width on mobile by default. */
  def apply(): Col = Col(12, 0, 0, 0, 0, Widget.empty)
  def apply(children: Widget*): Col = Col(12, 0, 0, 0, 0, Widget.fragment(children))
  def span(xs: Int): Col = Col(clamp(xs), 0, 0, 0, 0, Widget.empty)
}

object ColumnsStyle {

  /** CSS for 12-col grid; include in app `styleSheets`. */
  val sheet: StyleSheet = {
    def spanRules(prefix: String): String =
      (1 to 12).map { n =>
        s".$prefix-$n { grid-column: span $n; }"
      }.mkString("\n")

    val base =
      s""".oxy-row {
         |  display: grid;
         |  grid-template-columns: repeat(12, minmax(0, 1fr));
         |  width: 100%;
         |  box-sizing: border-box;
         |}
         |${spanRules("oxy-col-xs")}
         |""".stripMargin

    val sm = MediaCSS.smUp(spanRules("oxy-col-sm"))
    val md = MediaCSS.mdUp(spanRules("oxy-col-md"))
    val lg = MediaCSS.lgUp(spanRules("oxy-col-lg"))
    val xl = MediaCSS.xlUp(spanRules("oxy-col-xl"))

    StyleSheet.makeConst("oxygen-columns")(List(base, sm, md, lg, xl).mkString("\n"))
  }

}
