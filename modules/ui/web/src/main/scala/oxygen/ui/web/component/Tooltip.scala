package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W10-T04: hover/focus tooltip.
  *
  * Host is `inline-flex`; tip is absolutely centered under the trigger with
  * elevated inverse surface + caret. Visibility lives in [[sheet]] (include in app styleSheets).
  */
object Tooltip {

  /**
    * Wrap `trigger` with a hover/focus-revealed tip (below, centered).
    */
  def apply(tip: String)(trigger: Widget): Node =
    div(
      Widget.`class`("oxy-tooltip-host"),
      // keep host shrink-proof so absolute tip centers on real trigger width
      Widget.raw.css("min-width", "max-content"),
      Widget.raw.css("min-height", "max-content"),
      trigger,
      div(
        Widget.`class`("oxy-tooltip-tip"),
        Widget.raw.htmlAttr("role", "tooltip"),
        tip,
      ),
    )

  /**
    * Hover/focus show + elevated chrome. Include in app `styleSheets`.
    * Uses Oxygen CSS vars so light/dark stay consistent.
    */
  val sheet: StyleSheet =
    StyleSheet.makeConst("oxygen-tooltip")(
      s"""
        |.oxy-tooltip-host {
        |  position: relative;
        |  display: inline-flex;
        |  align-items: center;
        |  vertical-align: middle;
        |}
        |.oxy-tooltip-tip {
        |  position: absolute;
        |  left: 50%;
        |  top: calc(100% + 8px);
        |  transform: translateX(-50%);
        |  z-index: 1000;
        |  box-sizing: border-box;
        |  max-width: min(280px, 80vw);
        |  padding: 6px 10px;
        |  border-radius: 6px;
        |  background: var(${S.color.fg.default.name}, #e8e8e8);
        |  color: var(${S.color.fg.inverse.name}, #111);
        |  font-size: 12px;
        |  font-weight: 500;
        |  line-height: 1.35;
        |  letter-spacing: 0.01em;
        |  white-space: nowrap;
        |  text-align: center;
        |  pointer-events: none;
        |  box-shadow: 0 4px 14px rgba(0, 0, 0, 0.35), 0 0 0 1px rgba(0, 0, 0, 0.08);
        |  opacity: 0;
        |  visibility: hidden;
        |  transition: opacity 120ms ease, visibility 120ms ease;
        |}
        |.oxy-tooltip-tip::before {
        |  content: "";
        |  position: absolute;
        |  left: 50%;
        |  bottom: 100%;
        |  transform: translateX(-50%);
        |  border: 6px solid transparent;
        |  border-bottom-color: var(${S.color.fg.default.name}, #e8e8e8);
        |}
        |.oxy-tooltip-host:hover > .oxy-tooltip-tip,
        |.oxy-tooltip-host:focus-within > .oxy-tooltip-tip {
        |  opacity: 1;
        |  visibility: visible;
        |}
        |@media (prefers-reduced-motion: reduce) {
        |  .oxy-tooltip-tip { transition: none; }
        |}
        |""".stripMargin,
    )

  /**
    * Simple popover: open boolean on parent state drives visibility (click to toggle elsewhere).
    */
  def popover[S](isOpen: S => Boolean, content: Widget): WidgetS[S] =
    Widget.state[S].fix { st =>
      if isOpen(st.renderTimeValue) then
        div(
          position.absolute,
          zIndex := "200",
          padding := S.spacing._4,
          backgroundColor := S.color.bg.layerOne,
          border(1.px, "solid", S.color.bg.layerThree),
          borderRadius := S.borderRadius._4,
          boxShadow := "0 8px 24px rgba(0,0,0,0.18)",
          minWidth := 180.px,
          content,
        )
      else Widget.empty
    }

}
