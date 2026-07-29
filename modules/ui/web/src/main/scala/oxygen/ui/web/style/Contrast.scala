package oxygen.ui.web.style

import oxygen.ui.web.create.CSSColor

/**
  * W1-T09: pure WCAG-ish contrast helpers for authoring-time checks.
  * Does **not** gate builds; use in tests / gallery / theme tooling.
  */
object Contrast {

  /** WCAG relative luminance 0–1 for an sRGB color. */
  def relativeLuminance(c: CSSColor): Double = {
    val rgb = c match {
      case r: CSSColor.RGB     => r
      case CSSColor.RGBA(r, _) => r
    }
    def channel(v: Int): Double = {
      val s = v / 255.0
      if s <= 0.03928 then s / 12.92
      else math.pow((s + 0.055) / 1.055, 2.4)
    }
    0.2126 * channel(rgb.r.value) + 0.7152 * channel(rgb.g.value) + 0.0722 * channel(rgb.b.value)
  }

  def relativeLuminance(hex: String): Option[Double] =
    CSSColor.parse(hex).map(relativeLuminance)

  /**
    * Contrast ratio ≥ 1. (lighter + 0.05) / (darker + 0.05).
    */
  def ratio(a: CSSColor, b: CSSColor): Double = {
    val l1 = relativeLuminance(a)
    val l2 = relativeLuminance(b)
    val light = l1.max(l2)
    val dark = l1.min(l2)
    (light + 0.05) / (dark + 0.05)
  }

  def ratio(hexA: String, hexB: String): Option[Double] =
    for {
      a <- CSSColor.parse(hexA)
      b <- CSSColor.parse(hexB)
    } yield ratio(a, b)

  /** WCAG AA normal text. */
  val aaNormal: Double = 4.5

  /** WCAG AA large text / UI components. */
  val aaLarge: Double = 3.0

  /** Heuristic: hexes whose absolute lightness delta is below this are "too close". */
  val tooCloseLightnessDelta: Double = 0.08

  def meetsAaNormal(hexFg: String, hexBg: String): Boolean =
    ratio(hexFg, hexBg).exists(_ >= aaNormal)

  def meetsAaLarge(hexFg: String, hexBg: String): Boolean =
    ratio(hexFg, hexBg).exists(_ >= aaLarge)

  def tooClose(hexA: String, hexB: String): Boolean =
    (for {
      a <- CSSColor.parse(hexA)
      b <- CSSColor.parse(hexB)
      la = relativeLuminance(a)
      lb = relativeLuminance(b)
    } yield math.abs(la - lb) < tooCloseLightnessDelta).getOrElse(false)

  final case class Warning(pair: (String, String), ratio: Double, message: String)

  /**
    * Authoring report for default Oxygen seeds (fg/bg + primary on bg).
    * Empty list = no warnings under current thresholds.
    */
  def seedWarnings(seeds: OxygenColorSystem.Seeds): List[Warning] = {
    val pairs = List(
      ("foreground", seeds.foreground) -> (("background", seeds.background)),
      ("primary", seeds.primary) -> (("background", seeds.background)),
      ("danger", seeds.danger) -> (("background", seeds.background)),
    )
    pairs.flatMap { case ((n1, c1), (n2, c2)) =>
      ratio(c1, c2).toList.flatMap { r =>
        val msgs = List.newBuilder[Warning]
        if r < aaLarge then
          msgs += Warning((s"$n1=$c1", s"$n2=$c2"), r, f"contrast $r%.2f below AA large ($aaLarge)")
        if tooClose(c1, c2) then
          msgs += Warning((s"$n1=$c1", s"$n2=$c2"), r, "luminance too close (hard to distinguish)")
        msgs.result()
      }
    }
  }

}
