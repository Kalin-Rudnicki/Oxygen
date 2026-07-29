package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W10-T05: busy indicators — Spinner, Progress bar, Skeleton block.
  */
object Spinner {

  def apply(sizePx: Int = 24, label: String = ""): Node =
    div(
      display.inlineFlex,
      alignItems.center,
      gap := S.spacing._3,
      color := S.color.primary.standard,
      // rotating loader glyph (keyframes in Motion.sheet)
      span(
        display.inlineFlex,
        Widget.`class`("oxy-spin"),
        Icon.loader.size(sizePx),
      ),
      if label.nonEmpty then span(color := S.color.fg.moderate, label) else Widget.empty,
    )

  def small: Node = apply(16)
  def medium: Node = apply(24)
  def large: Node = apply(32)

}

object Progress {

  /** Pure clamp used by [[apply]] (unit-testable). */
  def clampFraction(fraction: Double): Double =
    fraction.max(0.0).min(1.0)

  /**
    * Determinate progress 0.0–1.0 (clamped). Token-styled track + fill.
    * @param fillColor CSS color for the filled portion (default primary).
    */
  def apply(
      fraction: Double,
      heightPx: Int = 8,
      fillColor: String = S.color.primary.standard,
  ): Node = {
    val f = clampFraction(fraction)
    div(
      width := 100.pct,
      height := heightPx.px,
      borderRadius := S.borderRadius._3,
      backgroundColor := S.color.bg.layerThree,
      overflow.hidden,
      div(
        height := 100.pct,
        width := s"${(f * 100).toInt}%",
        backgroundColor := fillColor,
        borderRadius := S.borderRadius._3,
        transition := "width var(--oxy-motion-duration-normal) var(--oxy-motion-easing-standard)",
      ),
    )
  }

  def percent(pct: Int, heightPx: Int = 8, fillColor: String = S.color.primary.standard): Node =
    apply(pct / 100.0, heightPx, fillColor)

}

object Skeleton {

  /** Placeholder block for loading content. */
  def block(w: String = "100%", h: String = 16.px): Node =
    div(
      width := w,
      height := h,
      borderRadius := S.borderRadius._2,
      backgroundColor := S.color.bg.layerTwo,
      opacity := "0.85",
      Widget.`class`("oxy-skeleton-pulse"),
    )

  def lines(n: Int = 3): Widget =
    div(
      display.flex,
      flexDirection.column,
      gap := S.spacing._2,
      Widget.foreach(List.range(0, n)) { i =>
        block(if i == n - 1 then "60%" else "100%", 12.px)
      },
    )

}
