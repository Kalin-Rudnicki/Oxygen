package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.{Scope, ZIO}

object StylesPage extends RoutablePage.NoParams[Any] {

  override type PageState = Unit

  override val path: Seq[String] = Seq("internal", "styles")

  override def title(state: Unit): String = "Styles"

  override def initialLoad(params: Unit): ZIO[Scope, UIError, Unit] = ZIO.unit

  override def postLoad(state: WidgetState[Unit], initialState: Unit): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: WidgetState[Unit], renderState: Unit): WidgetS[PageState] =
    fragment(
      h1("Styles"),
      OxygenStyleSheet.Scrollable,
      colorSection,
      div(height := 25.px),
      spacingSection,
      div(height := 25.px),
      borderWidthSection,
      div(height := 25.px),
      borderRadiusSection,
      div(height := 25.px),
      fontSizeSection,
      div(height := 25.px),
      colorMixing,
      div(height := 25.px),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val layerOne: Node =
    div(
      margin(0.px, 50.px),
      padding(10.px, 15.px),
      backgroundColor := OxygenStyleVars.color.bg.layerOne,
    )

  @scala.annotation.unused
  private val layerTwo: Node =
    div(
      padding(10.px, 15.px),
      backgroundColor := OxygenStyleVars.color.bg.layerTwo,
    )

  private val thCell: Node =
    th(backgroundColor := OxygenStyleVars.color.bg.default, padding(5.px, 15.px))

  private def colors(sectionName: String)(vars: CSSVar*): Widget =
    td(
      padding(10.px, 15.px),
      backgroundColor := OxygenStyleVars.color.bg.layerTwo,
      h3(sectionName, padding(0.px, 25.px)),
      verticalAlign := "top",
      table(
        tr(
          thCell("Color"),
          thCell(width := 350.px)("Name"),
        ),
        Widget.foreach(vars) { v =>
          tr(
            td(
              padding(0.px, 10.px),
              textAlign.center,
              span(
                display.inlineBlock,
                height := 35.px,
                width := 35.px,
                backgroundColor := v,
                border := "1px solid black",
                borderRadius := 10.px,
              ),
            ),
            td(
              paddingLeft := 25.px,
              v.name,
            ),
          )
        },
      ),
    )

  private val colorSection: Widget =
    layerOne(
      h2("Colors", padding(0.px, 25.px)),
      table(
        tr(
          colors("Core")(
            OxygenStyleVars.color.primary.standard,
            OxygenStyleVars.color.primary.strong,
            OxygenStyleVars.color.primary.subtle,
            OxygenStyleVars.color.primary.minimal,
          ),
          colors("Brand")(
            OxygenStyleVars.color.brand.primary1.light,
            OxygenStyleVars.color.brand.primary1.standard,
            OxygenStyleVars.color.brand.primary1.dark,
            OxygenStyleVars.color.brand.primary2.light,
            OxygenStyleVars.color.brand.primary2.standard,
            OxygenStyleVars.color.brand.primary2.dark,
          ),
        ),
        tr(
          colors("Foreground")(
            OxygenStyleVars.color.fg.default,
            OxygenStyleVars.color.fg.inverse,
            OxygenStyleVars.color.fg.moderate,
            OxygenStyleVars.color.fg.subtle,
            OxygenStyleVars.color.fg.minimal,
            OxygenStyleVars.color.fg.globalBlack,
            OxygenStyleVars.color.fg.globalWhite,
            OxygenStyleVars.color.fg.focus,
            OxygenStyleVars.color.fg.focusInverse,
            OxygenStyleVars.color.fg.textLink,
          ),
          colors("Background")(
            OxygenStyleVars.color.bg.default,
            OxygenStyleVars.color.bg.base,
            OxygenStyleVars.color.bg.layerOne,
            OxygenStyleVars.color.bg.layerTwo,
            OxygenStyleVars.color.bg.layerThree,
            OxygenStyleVars.color.bg.transparent,
          ),
        ),
        tr(
          colors("Highlight")(
            OxygenStyleVars.color.highlight.accent.standard,
            OxygenStyleVars.color.highlight.accent.strong,
            OxygenStyleVars.color.highlight.accent.subtle,
            OxygenStyleVars.color.highlight.accent.minimal,
            OxygenStyleVars.color.highlight.brand,
            OxygenStyleVars.color.highlight._1,
            OxygenStyleVars.color.highlight._2,
            OxygenStyleVars.color.highlight._3,
            OxygenStyleVars.color.highlight._4,
            OxygenStyleVars.color.highlight._5,
          ),
          colors("Status")(
            OxygenStyleVars.color.status.positive.standard,
            OxygenStyleVars.color.status.positive.strong,
            OxygenStyleVars.color.status.positive.subtle,
            OxygenStyleVars.color.status.positive.minimal,
            OxygenStyleVars.color.status.negative.standard,
            OxygenStyleVars.color.status.negative.strong,
            OxygenStyleVars.color.status.negative.subtle,
            OxygenStyleVars.color.status.negative.minimal,
            OxygenStyleVars.color.status.alert.standard,
            OxygenStyleVars.color.status.alert.strong,
            OxygenStyleVars.color.status.alert.subtle,
            OxygenStyleVars.color.status.alert.minimal,
            OxygenStyleVars.color.status.informational.standard,
            OxygenStyleVars.color.status.informational.strong,
            OxygenStyleVars.color.status.informational.subtle,
            OxygenStyleVars.color.status.informational.minimal,
            OxygenStyleVars.color.status.notification,
          ),
        ),
      ),
    )

  private val spacingSection: Widget = {
    val vars: Seq[CSSVar] =
      Seq(
        OxygenStyleVars.spacing._1px,
        OxygenStyleVars.spacing._2px,
        OxygenStyleVars.spacing.xxs,
        OxygenStyleVars.spacing.xs,
        OxygenStyleVars.spacing.s,
        OxygenStyleVars.spacing.m,
        OxygenStyleVars.spacing.l,
        OxygenStyleVars.spacing.xl,
        OxygenStyleVars.spacing.xxl,
        OxygenStyleVars.spacing._0,
        OxygenStyleVars.spacing._1,
        OxygenStyleVars.spacing._2,
        OxygenStyleVars.spacing._3,
        OxygenStyleVars.spacing._4,
        OxygenStyleVars.spacing._5,
        OxygenStyleVars.spacing._6,
        OxygenStyleVars.spacing._7,
        OxygenStyleVars.spacing._8,
        OxygenStyleVars.spacing._9,
        OxygenStyleVars.spacing._10,
        OxygenStyleVars.spacing._11,
        OxygenStyleVars.spacing._12,
        OxygenStyleVars.spacing._13,
        OxygenStyleVars.spacing._14,
        OxygenStyleVars.spacing._15,
        OxygenStyleVars.spacing._16,
        OxygenStyleVars.spacing._17,
        OxygenStyleVars.spacing._18,
        OxygenStyleVars.spacing._19,
        OxygenStyleVars.spacing._20,
        OxygenStyleVars.spacing._21,
        OxygenStyleVars.spacing._22,
        OxygenStyleVars.spacing._23,
        OxygenStyleVars.spacing._24,
        OxygenStyleVars.spacing._25,
      )

    layerOne(
      h2("Spacing", padding(0.px, 25.px)),
      table(
        tr(
          thCell("Spacing"),
          thCell(width := 200.px)("Name"),
        ),
        Widget.foreach(vars) { v =>
          tr(
            td(
              textAlign.center,
              span(
                display.inlineBlock,
                height := 20.px,
                width := v,
                backgroundColor := OxygenStyleVars.color.primary,
              ),
            ),
            td(
              paddingLeft := 25.px,
              v.name,
            ),
          )
        },
      ),
    )
  }

  private val borderWidthSection: Widget = {
    val vars: Seq[CSSVar] =
      Seq(
        OxygenStyleVars.borderWidth._0,
        OxygenStyleVars.borderWidth._1,
        OxygenStyleVars.borderWidth._2,
        OxygenStyleVars.borderWidth._3,
        OxygenStyleVars.borderWidth._4,
        OxygenStyleVars.borderWidth._5,
        OxygenStyleVars.borderWidth._6,
      )

    layerOne(
      h2("Border Width", padding(0.px, 25.px)),
      table(
        tr(
          thCell("Border Width"),
          thCell(width := 200.px)("Name"),
        ),
        Widget.foreach(vars) { v =>
          tr(
            td(
              padding(0.px, 10.px),
              textAlign.center,
              span(
                display.inlineBlock,
                height := 50.px,
                width := 75.px,
                backgroundColor := OxygenStyleVars.color.bg.base,
                borderWidth := v,
                borderColor.white,
                borderStyle.solid,
              ),
            ),
            td(
              paddingLeft := 25.px,
              v.name,
            ),
          )
        },
      ),
    )
  }

  private val borderRadiusSection: Widget = {
    val vars: Seq[CSSVar] =
      Seq(
        OxygenStyleVars.borderRadius._1px,
        OxygenStyleVars.borderRadius._2px,
        OxygenStyleVars.borderRadius.s,
        OxygenStyleVars.borderRadius.m,
        OxygenStyleVars.borderRadius.l,
        OxygenStyleVars.borderRadius._0,
        OxygenStyleVars.borderRadius._1,
        OxygenStyleVars.borderRadius._2,
        OxygenStyleVars.borderRadius._3,
        OxygenStyleVars.borderRadius._4,
        OxygenStyleVars.borderRadius._5,
        OxygenStyleVars.borderRadius._6,
        OxygenStyleVars.borderRadius._7,
        OxygenStyleVars.borderRadius._8,
      )

    layerOne(
      h2("Border Radius", padding(0.px, 25.px)),
      table(
        tr(
          thCell("Border Radius"),
          thCell(width := 200.px)("Name"),
        ),
        Widget.foreach(vars) { v =>
          tr(
            td(
              padding(0.px, 10.px),
              textAlign.center,
              span(
                display.inlineBlock,
                height := 75.px,
                width := 100.px,
                backgroundColor := OxygenStyleVars.color.primary,
                borderRadius := v,
              ),
              span(
                display.inlineBlock,
                width := 15.px,
              ),
              span(
                display.inlineBlock,
                height := 35.px,
                width := 50.px,
                backgroundColor := OxygenStyleVars.color.primary,
                borderRadius := v,
              ),
            ),
            td(
              paddingLeft := 25.px,
              v.name,
            ),
          )
        },
      ),
    )
  }

  private val fontSizeSection: Widget = {
    val vars: Seq[CSSVar] =
      Seq(
        OxygenStyleVars.fontSize._1,
        OxygenStyleVars.fontSize._2,
        OxygenStyleVars.fontSize._3,
        OxygenStyleVars.fontSize._4,
        OxygenStyleVars.fontSize._5,
        OxygenStyleVars.fontSize._6,
        OxygenStyleVars.fontSize._7,
        OxygenStyleVars.fontSize._8,
        OxygenStyleVars.fontSize._9,
        OxygenStyleVars.fontSize._10,
        OxygenStyleVars.fontSize._11,
        OxygenStyleVars.fontSize._12,
        OxygenStyleVars.fontSize._13,
        OxygenStyleVars.fontSize._14,
        OxygenStyleVars.fontSize._15,
      )

    layerOne(
      h2("Font Size", padding(0.px, 25.px)),
      table(
        tr(
          thCell("Font Size"),
          thCell(width := 200.px)("Name"),
        ),
        Widget.foreach(vars) { v =>
          tr(
            td(
              padding(0.px, 10.px),
              textAlign.center,
              "Some Text",
              fontSize := v,
            ),
            td(
              paddingLeft := 25.px,
              v.name,
            ),
          )
        },
      ),
    )
  }

  private lazy val colorMixing: Widget = {
    val colorCellBase: Node =
      td(width := 50.px, height := 50.px, textAlign.center)

    val percents: Seq[Double] =
      Seq(10.0, 25.0, 50.0, 75.0, 90.0, 100.0)

    def labelText(s: String, fs: Int = 10): Widget =
      div(
        backgroundColor := "#0008",
        color.white,
        padding(S.spacing._1, S.spacing._2),
        margin(S.spacing._2, S.spacing._1),
        borderRadius := S.borderRadius._3,
        fontSize := fs.px,
      )(s)

    def colorCell(c: CSSColor, text: String): Widget =
      colorCellBase(backgroundColor := c, labelText(text), labelText(c.toString))

    def row(c: CSSColor): Widget =
      tr(
        border(2.px, "solid", "black"),
        td(textAlign.center, padding(S.spacing._2, S.spacing._5))(c.toString),
        //
        Widget.foreach(percents.reverse) { p => colorCell(c.darken(p), s"-$p%") },
        colorCell(c, "base"),
        Widget.foreach(percents) { p => colorCell(c.lighten(p), s"+$p%") },
        td(width := 10.px),
        Widget.foreach(percents.reverse) { p => colorCell(c.decreaseLightness(p), s"-$p%") },
        colorCell(c, "base"),
        Widget.foreach(percents) { p => colorCell(c.increaseLightness(p), s"+$p%") },
      )

    SectionWithHeader.section1("Color Mixing")(
      table(
        borderCollapse.collapse,
        tr(
          border(2.px, "solid", "black"),
          th("Hex Color", padding(S.spacing._2, S.spacing._5)),
          th(
            padding(S.spacing._2, S.spacing._5),
            border(2.px, "solid", "black"),
            colSpan := (percents.size * 2 + 1),
            "darken / lighten",
          ),
          th(),
          th(
            padding(S.spacing._2, S.spacing._5),
            border(2.px, "solid", "black"),
            colSpan := (percents.size * 2 + 1),
            "decreaseLightness / increaseLightness",
          ),
        ),
        tr(height := 10.px),
        row(S.color.primary.standard.getColorValue),
        row(S.color.primary.strong.getColorValue),
        row(S.color.primary.subtle.getColorValue),
        row(S.color.primary.minimal.getColorValue),
        row(S.color.status.positive.standard.getColorValue),
        row(S.color.status.positive.strong.getColorValue),
        row(S.color.status.positive.subtle.getColorValue),
        row(S.color.status.positive.minimal.getColorValue),
        row(S.color.status.negative.standard.getColorValue),
        row(S.color.status.negative.strong.getColorValue),
        row(S.color.status.negative.subtle.getColorValue),
        row(S.color.status.negative.minimal.getColorValue),
        tr(height := 25.px),
        row(CSSColor("#000")),
        row(CSSColor("#F00")),
        row(CSSColor("#0F0")),
        row(CSSColor("#00F")),
        row(CSSColor("#FF0")),
        row(CSSColor("#0FF")),
        row(CSSColor("#F0F")),
        row(CSSColor("#FFF")),
        tr(height := 10.px),
        row(CSSColor("#000")),
        row(CSSColor("#800")),
        row(CSSColor("#080")),
        row(CSSColor("#008")),
        row(CSSColor("#880")),
        row(CSSColor("#088")),
        row(CSSColor("#808")),
        row(CSSColor("#888")),
        tr(height := 10.px),
        row(CSSColor("#4A0")),
        row(CSSColor("#A40")),
        row(CSSColor("#04A")),
        row(CSSColor("#0A4")),
        row(CSSColor("#40A")),
        row(CSSColor("#A04")),
        tr(height := 10.px),
        row(CSSColor("#4A8")),
        row(CSSColor("#A48")),
        row(CSSColor("#84A")),
        row(CSSColor("#8A4")),
        row(CSSColor("#48A")),
        row(CSSColor("#A84")),
      ),
    )
  }

}
