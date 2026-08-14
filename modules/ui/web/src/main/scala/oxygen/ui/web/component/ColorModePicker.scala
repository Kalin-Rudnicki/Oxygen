package oxygen.ui.web.component

import org.scalajs.dom.window
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.ColorMode

/**
  * Reusable, drop-in color-mode picker (Light / Dark / System).
  *
  * Self-contained: backed by a shared [[GlobalState]] seeded from the stored preference, so it
  * reflects the current mode and can be dropped into any page without wiring page state. Clicking
  * an option calls [[ColorMode.setAndPersist]] (persists + rebinds `data-color-mode` + notifies
  * other tabs) and updates the highlight.
  *
  * a11y: `role=radiogroup` container, `role=radio` + `aria-checked` per option.
  *
  * {{{
  * ColorModePicker()                 // just the segmented control
  * ColorModePicker(label = "Color mode".some)
  * }}}
  *
  * Known limitation (by design, OXY-154): the highlight does not live-update on cross-tab /
  * programmatic mode changes until the page re-renders. Cross-tab *application* still works via
  * `ColorTheme.install` / `ColorMode.subscribeCrossTab`.
  */
object ColorModePicker {

  private val options: Seq[ColorMode.Mode] =
    Seq(ColorMode.Mode.Light, ColorMode.Mode.Dark, ColorMode.Mode.System)

  private def readStored: ColorMode.Mode =
    Option(window.localStorage.getItem(ColorMode.storageKey)).flatMap(ColorMode.parse).getOrElse(ColorMode.Mode.System)

  private object ModeState extends GlobalState[ColorMode.Mode]("ColorModePicker")(readStored)

  private def option(st: WidgetState[ColorMode.Mode], mode: ColorMode.Mode, current: ColorMode.Mode): Widget = {
    val selected: Boolean = mode == current
    span(
      Widget.raw.htmlAttr("role", "radio"),
      Widget.raw.htmlAttr("aria-checked", if selected then "true" else "false"),
      Widget.raw.htmlAttr("tabindex", if selected then "0" else "-1"),
      display.inlineFlex,
      alignItems.center,
      cursor.pointer,
      userSelect.none,
      padding(S.spacing._1, S.spacing._3),
      fontSize := S.fontSize._2,
      fontWeight := S.fontWeight.semiBold,
      borderStyle.solid,
      borderWidth := 1.px,
      if selected then
        fragment(
          color.dynamic := S.color.primary.on,
          backgroundColor.dynamic := S.color.primary.standard,
          borderColor.dynamic := S.color.primary.standard,
        )
      else
        fragment(
          color.dynamic := S.color.fg.default,
          backgroundColor.dynamic := S.color.bg.layerTwo,
          backgroundColor.dynamic.hover := S.color.bg.layerThree,
          borderColor.dynamic := S.color.bg.layerThree,
        ),
      mode.pretty,
      onClick := (ColorMode.setAndPersist(mode) *> st.set(mode)),
    )
  }

  /** Segmented Light / Dark / System control. Pass `label` to render a leading caption. */
  def apply(label: Option[String] = None): Widget =
    Widget.state[ColorMode.Mode].detach(ModeState) { st =>
      val current: ColorMode.Mode = st.renderTimeValue
      div(
        display.inlineFlex,
        alignItems.center,
        gap := S.spacing._2,
        flexWrap.wrap,
        label match {
          case Some(text) => span(text, color := S.color.fg.moderate, fontSize := S.fontSize._2)
          case None       => Widget.empty
        },
        div(
          Widget.raw.htmlAttr("role", "radiogroup"),
          Widget.raw.htmlAttr("aria-label", "Color mode"),
          display.inlineFlex,
          borderRadius := S.borderRadius._3,
          overflow.hidden,
          Widget.foreach(options)(option(st, _, current)),
        ),
      )
    }

}
