package oxygen.core

import oxygen.core.syntax.option.*

// TODO (KR) : remove - replaced by new LazyString
final case class ColorState(fg: Option[Color], bg: Option[Color])
object ColorState {

  val empty: ColorState = ColorState(None, None)

  def fg(color: Color): ColorState = ColorState(color.some, None)
  def bg(color: Color): ColorState = ColorState(None, color.some)

  final case class Concrete private (colorMode: ColorMode.NonColorless, fg: Option[Color.Concrete], bg: Option[Color.Concrete]) { outer =>

    def diff(inner: ColorState): (String, String, ColorState.Concrete) = {
      def newColor(outer: Option[Color.Concrete], inner: Option[Color], mod: Color.Concrete => String): (Option[String], Option[String], Option[Color.Concrete]) = {
        val o = outer
        val i = inner.flatMap(colorMode.toConcrete)
        (outer, i) match
          case (None, Some(inner))                          => (mod(inner).some, mod(Color.Default).some, i.orElse(o))
          case (Some(outer), Some(inner)) if outer != inner => (mod(inner).some, mod(outer).some, i.orElse(o))
          case _                                            => (None, None, i.orElse(o))
      }

      def joinMods(mods: Option[String]*): String = {
        val flat = mods.flatten
        if flat.isEmpty then ""
        else s"${colorMode.prefix}${flat.mkString(";")}${colorMode.suffix}"
      }

      val (fgMod, fgRevert, newFgColor) = newColor(outer.fg, inner.fg, colorMode.fgMod)
      val (bgMod, bgRevert, newBgColor) = newColor(outer.bg, inner.bg, colorMode.bgMod)
      (
        joinMods(fgMod, bgMod),
        joinMods(fgRevert, bgRevert),
        ColorState.Concrete(colorMode, newFgColor, newBgColor),
      )
    }

  }
  object Concrete {

    def apply(colorMode: ColorMode.NonColorless, colorState: ColorState): ColorState.Concrete =
      ColorState.Concrete(colorMode, colorState.fg.flatMap(colorMode.toConcrete), colorState.bg.flatMap(colorMode.toConcrete))

  }

}
