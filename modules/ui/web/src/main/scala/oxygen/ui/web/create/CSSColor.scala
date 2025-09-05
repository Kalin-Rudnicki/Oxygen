package oxygen.ui.web.create

import oxygen.core.Unapply
import oxygen.predef.core.*
import scala.util.Try

sealed trait CSSColor {

  final def |(that: CSSColor): CSSColor = CSSColor.mix(this)(that)
  final def +(that: CSSColor): CSSColor = CSSColor.add(this)(that)
  final def -(that: CSSColor): CSSColor = CSSColor.sub(this)(that)

  final def mix(that: CSSColor): CSSColor = CSSColor.mix(this)(that)
  final def mix(w1: Int)(that: CSSColor, w2: Int): CSSColor = CSSColor.mix(this, w1)(that, w2)

  // TODO (KR) : add opacity
  // TODO (KR) : remove opacity

  final def modifyRgb(f: CSSColor.RGB => CSSColor.RGB): CSSColor = this match
    case rgb: CSSColor.RGB     => f(rgb)
    case CSSColor.RGBA(rgb, a) => CSSColor.RGBA(f(rgb), a)

  /**
    * Direct modifier of lightness. You most likely want [[darken]].
    */
  final def decreaseLightness(percent: Double): CSSColor =
    this.modifyRgb { rgb =>
      val hsl = rgb.toHSL
      hsl.copy(lightness = hsl.lightness * (1.0 - CSSColor.normalizePercent(percent))).toRGB
    }

  /**
    * Direct modifier of lightness. You most likely want [[lighten]].
    */
  final def increaseLightness(percent: Double): CSSColor =
    this.modifyRgb { rgb =>
      val hsl = rgb.toHSL
      hsl.copy(lightness = hsl.lightness * (1.0 + CSSColor.normalizePercent(percent))).toRGB
    }

  /**
    * Will place the color the provided [[percent]] of the way between current lightness and black.
    */
  final def darken(percent: Double): CSSColor =
    this.modifyRgb { rgb =>
      val hsl = rgb.toHSL
      hsl.copy(lightness = CSSColor.percentRange(0.0, hsl.lightness, 1.0 - CSSColor.normalizePercent(percent))).toRGB
    }

  /**
    * Will place the color the provided [[percent]] of the way between current lightness and white.
    */
  final def lighten(percent: Double): CSSColor =
    this.modifyRgb { rgb =>
      val hsl = rgb.toHSL
      hsl.copy(lightness = CSSColor.percentRange(hsl.lightness, 1.0, CSSColor.normalizePercent(percent))).toRGB
    }

  final def setOpacity(percent: Double): CSSColor = this match
    case rgb: CSSColor.RGB     => CSSColor.RGBA(rgb, CSSColor.HexPair.fromPercent(percent))
    case CSSColor.RGBA(rgb, _) => CSSColor.RGBA(rgb, CSSColor.HexPair.fromPercent(percent))

  final lazy val show: String = this match
    case CSSColor.RGB(r, g, b) => s"#$r$g$b"
    case CSSColor.RGBA(rgb, a) => s"$rgb$a"

  override final def toString: String = show

}
object CSSColor {

  private def percentRange(min: Double, max: Double, percent: Double): Double =
    min + (max - min) * percent

  final case class RGB(r: CSSColor.HexPair, g: CSSColor.HexPair, b: CSSColor.HexPair) extends CSSColor {

    def toHSL: HSL = {
      val rNorm = r.value / 255.0
      val gNorm = g.value / 255.0
      val bNorm = b.value / 255.0

      val max = rNorm.max(gNorm).max(bNorm)
      val min = rNorm.min(gNorm).min(bNorm)
      val delta = max - min

      // Hue calculation
      val hue =
        if (delta == 0) 0.0
        else if (max == rNorm) (60 * ((gNorm - bNorm) / delta) + 360) % 360
        else if (max == gNorm) (60 * ((bNorm - rNorm) / delta) + 120) % 360
        else (60 * ((rNorm - gNorm) / delta) + 240) % 360

      // Lightness calculation
      val lightness = (max + min) / 2

      // Saturation calculation
      val saturation =
        if (delta == 0) 0.0
        else delta / (1 - Math.abs(2 * lightness - 1))

      HSL(hue, saturation, lightness)
    }

  }

  final case class RGBA(rgb: CSSColor.RGB, a: CSSColor.HexPair) extends CSSColor

  final case class HSL(hue: Double, saturation: Double, lightness: Double) {

    def h: Double = hue
    def s: Double = saturation
    def l: Double = lightness

    def toRGB: CSSColor.RGB = {
      val c = (1 - Math.abs(2 * lightness - 1)) * saturation
      val x = c * (1 - Math.abs((hue / 60) % 2 - 1))
      val m = lightness - c / 2

      val (r1, g1, b1) =
        if (hue < 60) (c, x, 0.0)
        else if (hue < 120) (x, c, 0.0)
        else if (hue < 180) (0.0, c, x)
        else if (hue < 240) (0.0, x, c)
        else if (hue < 300) (x, 0.0, c)
        else (c, 0.0, x)

      val r = ((r1 + m) * 255).toInt
      val g = ((g1 + m) * 255).toInt
      val b = ((b1 + m) * 255).toInt

      RGB(HexPair.fromIntBound(r), HexPair.fromIntBound(g), HexPair.fromIntBound(b))
    }

  }

  /**
    * Accept a Double between 0-100.
    * Will handle values in the following way:
    * - 100.0 -> 100%
    * - 1.0   -> 100%
    * - 0.0   -> 0%
    * - 1.5   -> 1.5%
    * - 150.0   -> 100%
    * - -10.0   -> 0%
    */
  private def normalizePercent(percent: Double): Double =
    if (percent > 100.0) 1.0
    else if (percent < 0) 0.0
    else if (percent <= 1.0) percent
    else percent / 100

  def mixRGB(c1: CSSColor.RGB, w1: Int)(c2: CSSColor.RGB, w2: Int): CSSColor.RGB =
    CSSColor.RGB(
      HexPair.mix(c1.r, w1)(c2.r, w2),
      HexPair.mix(c1.g, w1)(c2.g, w2),
      HexPair.mix(c1.b, w1)(c2.b, w2),
    )

  def mix(c1: CSSColor, w1: Int)(c2: CSSColor, w2: Int): CSSColor =
    (c1, c2) match
      case (rgb1: CSSColor.RGB, rgb2: CSSColor.RGB)           => mixRGB(rgb1, w1)(rgb2, w2)
      case (rgb1: CSSColor.RGB, CSSColor.RGBA(rgb2, a2))      => CSSColor.RGBA(mixRGB(rgb1, w1)(rgb2, w2), HexPair.mix(HexPair._255, w1)(a2, w2))
      case (CSSColor.RGBA(rgb1, a1), rgb2: CSSColor.RGB)      => CSSColor.RGBA(mixRGB(rgb1, w1)(rgb2, w2), HexPair.mix(a1, w1)(HexPair._255, w2))
      case (CSSColor.RGBA(rgb1, a1), CSSColor.RGBA(rgb2, a2)) => CSSColor.RGBA(mixRGB(rgb1, w1)(rgb2, w2), HexPair.mix(a1, w1)(a2, w2))

  def mix(c1: CSSColor)(c2: CSSColor): CSSColor =
    mix(c1, 1)(c2, 1)

  def addRGB(c1: CSSColor.RGB)(c2: CSSColor.RGB): CSSColor.RGB =
    CSSColor.RGB(
      c1.r + c2.r,
      c1.g + c2.g,
      c1.b + c2.b,
    )

  def add(c1: CSSColor)(c2: CSSColor): CSSColor =
    (c1, c2) match
      case (rgb1: CSSColor.RGB, rgb2: CSSColor.RGB)           => addRGB(rgb1)(rgb2)
      case (rgb1: CSSColor.RGB, CSSColor.RGBA(rgb2, a2))      => CSSColor.RGBA(addRGB(rgb1)(rgb2), a2)
      case (CSSColor.RGBA(rgb1, a1), rgb2: CSSColor.RGB)      => CSSColor.RGBA(addRGB(rgb1)(rgb2), a1)
      case (CSSColor.RGBA(rgb1, a1), CSSColor.RGBA(rgb2, a2)) => CSSColor.RGBA(addRGB(rgb1)(rgb2), a1 + a2)

  def subRgb(c1: CSSColor.RGB)(c2: CSSColor.RGB): CSSColor.RGB =
    CSSColor.RGB(
      c1.r - c2.r,
      c1.g - c2.g,
      c1.b - c2.b,
    )

  def sub(c1: CSSColor)(c2: CSSColor): CSSColor =
    (c1, c2) match
      case (rgb1: CSSColor.RGB, rgb2: CSSColor.RGB)           => subRgb(rgb1)(rgb2)
      case (rgb1: CSSColor.RGB, CSSColor.RGBA(rgb2, a2))      => CSSColor.RGBA(subRgb(rgb1)(rgb2), a2)
      case (CSSColor.RGBA(rgb1, a1), rgb2: CSSColor.RGB)      => CSSColor.RGBA(subRgb(rgb1)(rgb2), a1)
      case (CSSColor.RGBA(rgb1, a1), CSSColor.RGBA(rgb2, a2)) => CSSColor.RGBA(subRgb(rgb1)(rgb2), a1 - a2)

  // 00 - FF
  final case class HexPair(value: Int) {

    def +(that: HexPair): HexPair = HexPair((this.value + that.value).min(255))
    def -(that: HexPair): HexPair = HexPair((this.value + that.value).max(0))

    def *(mult: Double): HexPair = HexPair.fromIntBound((value * mult).toInt)

    lazy val show: String = Integer.toString(value, 16).alignRight(2, '0')

    override def toString: String = show

  }
  object HexPair {

    val _0: HexPair = HexPair(0)
    val _255: HexPair = HexPair(255)

    def mix(p1: HexPair, w1: Int)(p2: HexPair, w2: Int): HexPair =
      HexPair((p1.value * w1 + p2.value * w2) / (w1 + w2))

    def mix(p1: HexPair)(p2: HexPair): HexPair =
      mix(p1, 1)(p2, 1)

    def fromIntOption(int: Int): Option[HexPair] =
      Option.when(int >= 0 && int <= 255)(HexPair(int))

    def fromIntBound(int: Int): HexPair =
      HexPair(
        if (int > 255) 255
        else if (int < 0) 0
        else int,
      )

    def fromPercent(percent: Double): HexPair =
      HexPair((255 * normalizePercent(percent)).toInt)

  }

  // TODO (KR) : macro
  def apply(string: String): CSSColor =
    unsafeParse(string)

  def apply(cssVar: CSSVar): CSSColor =
    eval(cssVar.varString)

  def eval(string: String): CSSColor =
    unsafeParse(CSSVar.eval(string))

  def unsafeParse(string: String): CSSColor =
    parse(string).getOrElse(throw new RuntimeException(s"Invalid color string: '$string'"))

  private val regRgb = "^[ ]*rgb\\([ ]*([0-9]{1,3})(?:,[ ]*|[ ]+)([0-9]{1,3})(?:,[ ]*|[ ]+)([0-9]{1,3})[ ]*\\)[ ]*$".r
  private val regHex3 = "^[ ]*#([0-9a-f])([0-9a-f])([0-9a-f])[ ]*$".r
  private val regHex4 = "^[ ]*#([0-9a-f])([0-9a-f])([0-9a-f])([0-9a-f])[ ]*$".r
  private val regHex6 = "^[ ]*#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})[ ]*$".r
  private val regHex8 = "^[ ]*#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})[ ]*$".r

  private val int_00_ff: Unapply[String, HexPair] =
    str => Try { Integer.parseInt(str, 16) }.toOption.flatMap(HexPair.fromIntOption)

  private val int_0_f: Unapply[String, HexPair] =
    str => int_00_ff.unapply(str + str)

  private val int_0_255: Unapply[String, HexPair] =
    str => Try { Integer.parseInt(str, 10) }.toOption.flatMap(HexPair.fromIntOption)

  def parse(string: String): Option[CSSColor] = string.toLowerCase match
    case regHex6(int_00_ff(r), int_00_ff(g), int_00_ff(b))               => RGB(r, g, b).some
    case regHex8(int_00_ff(r), int_00_ff(g), int_00_ff(b), int_00_ff(a)) => RGBA(RGB(r, g, b), a).some
    case regHex3(int_0_f(r), int_0_f(g), int_0_f(b))                     => RGB(r, g, b).some
    case regHex4(int_0_f(r), int_0_f(g), int_0_f(b), int_0_f(a))         => RGBA(RGB(r, g, b), a).some
    case regRgb(int_0_255(r), int_0_255(g), int_0_255(b))                => RGB(r, g, b).some
    // TODO (KR) : rgba()
    case _ => None

}
