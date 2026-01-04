package oxygen.core

import oxygen.core.syntax.option.*

sealed trait ColorStateV2 {

  val apply: String
  val revert: String

  final def patchFg(colorMode: ColorMode, newFg: Color): Option[ColorStateV2.Patch] =
    colorMode match
      case ColorMode.Colorless               => None
      case colorMode: ColorMode.NonColorless =>
        colorMode.toConcrete(newFg).filter(fgChanged) match
          case Some(newFg) => patchFg(colorMode, newFg)
          case None        => None
  final def patchBg(colorMode: ColorMode, newBg: Color): Option[ColorStateV2.Patch] =
    colorMode match
      case ColorMode.Colorless               => None
      case colorMode: ColorMode.NonColorless =>
        colorMode.toConcrete(newBg).filter(bgChanged) match
          case Some(newBg) => patchBg(colorMode, newBg)
          case None        => None
  final def patchFgBg(colorMode: ColorMode, newFg: Color, newBg: Color): Option[ColorStateV2.Patch] =
    colorMode match
      case ColorMode.Colorless               => None
      case colorMode: ColorMode.NonColorless =>
        (colorMode.toConcrete(newFg).filter(fgChanged), colorMode.toConcrete(newBg).filter(bgChanged)) match
          case (Some(newFg), Some(newBg)) => patchFgBg(colorMode, newFg, newBg)
          case (Some(newFg), None)        => patchFg(colorMode, newFg)
          case (None, Some(newBg))        => patchBg(colorMode, newBg)
          case (None, None)               => None

  protected def fgChanged(newFg: Color.Concrete): Boolean
  protected def bgChanged(newBg: Color.Concrete): Boolean

  def patchFg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete): Option[ColorStateV2.Patch]
  def patchBg(colorMode: ColorMode.NonColorless, newBg: Color.Concrete): Option[ColorStateV2.Patch]
  def patchFgBg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete, newBg: Color.Concrete): Option[ColorStateV2.Patch]

}
object ColorStateV2 {

  final case class Patch(
      newState: ColorStateV2,
      apply: String,
      revert: String,
  )

  case object Empty extends ColorStateV2 {
    override val apply: String = ""
    override val revert: String = ""
    override protected def fgChanged(newFg: Color.Concrete): Boolean = newFg != Color.Default
    override protected def bgChanged(newBg: Color.Concrete): Boolean = newBg != Color.Default
    override def patchFg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFg(colorMode, newFg), applyFg(colorMode, newFg), applyFg(colorMode, Color.Default)).some
    override def patchBg(colorMode: ColorMode.NonColorless, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedBg(colorMode, newBg), applyBg(colorMode, newBg), applyBg(colorMode, Color.Default)).some
    override def patchFgBg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, Color.Default, Color.Default)).some
  }

  sealed trait NonEmpty extends ColorStateV2 {
    // TODO (KR) : note, there is an implicit assumption here that only the same colorMode will ever be compared
    //           : a full solution would support de-colorizing one color more, and re-colorizing into another
    //           :
    val colorMode: ColorMode.NonColorless
  }

  final case class ColorizedFg(colorMode: ColorMode.NonColorless, fg: Color.Concrete) extends ColorStateV2.NonEmpty {
    override val apply: String = applyFg(colorMode, fg)
    override val revert: String = applyFg(colorMode, Color.Default)
    override protected def fgChanged(newFg: Color.Concrete): Boolean = newFg != fg
    override protected def bgChanged(newBg: Color.Concrete): Boolean = newBg != Color.Default
    override def patchFg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFg(colorMode, newFg), applyFg(colorMode, newFg), applyFg(colorMode, fg)).some
    override def patchBg(colorMode: ColorMode.NonColorless, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, fg, newBg), applyBg(colorMode, newBg), applyBg(colorMode, Color.Default)).some
    override def patchFgBg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, fg, Color.Default)).some
  }

  final case class ColorizedBg(colorMode: ColorMode.NonColorless, bg: Color.Concrete) extends ColorStateV2.NonEmpty {
    override val apply: String = applyBg(colorMode, bg)
    override val revert: String = applyBg(colorMode, Color.Default)
    override protected def fgChanged(newFg: Color.Concrete): Boolean = newFg != Color.Default
    override protected def bgChanged(newBg: Color.Concrete): Boolean = newBg != bg
    override def patchFg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, newFg, bg), applyFg(colorMode, newFg), applyFg(colorMode, Color.Default)).some
    override def patchBg(colorMode: ColorMode.NonColorless, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedBg(colorMode, newBg), applyBg(colorMode, newBg), applyBg(colorMode, Color.Default)).some
    override def patchFgBg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, Color.Default, bg)).some
  }

  final case class ColorizedFgBg(colorMode: ColorMode.NonColorless, fg: Color.Concrete, bg: Color.Concrete) extends ColorStateV2.NonEmpty {
    override val apply: String = applyFgBg(colorMode, fg, bg)
    override val revert: String = applyFgBg(colorMode, Color.Default, Color.Default)
    override protected def fgChanged(newFg: Color.Concrete): Boolean = newFg != fg
    override protected def bgChanged(newBg: Color.Concrete): Boolean = newBg != bg
    override def patchFg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, newFg, bg), applyFg(colorMode, newFg), applyFg(colorMode, fg)).some
    override def patchBg(colorMode: ColorMode.NonColorless, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, fg, newBg), applyBg(colorMode, newBg), applyBg(colorMode, bg)).some
    override def patchFgBg(colorMode: ColorMode.NonColorless, newFg: Color.Concrete, newBg: Color.Concrete): Option[Patch] =
      Patch(ColorizedFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, newFg, newBg), applyFgBg(colorMode, fg, bg)).some
  }

  // TODO (KR) : be more efficient
  private def applyFg(colorMode: ColorMode.NonColorless, fg: Color.Concrete): String = colorMode.prefix + colorMode.fgMod(fg) + colorMode.suffix
  private def applyBg(colorMode: ColorMode.NonColorless, bg: Color.Concrete): String = colorMode.prefix + colorMode.bgMod(bg) + colorMode.suffix
  private def applyFgBg(colorMode: ColorMode.NonColorless, fg: Color.Concrete, bg: Color.Concrete): String = colorMode.prefix + colorMode.fgMod(fg) + ";" + colorMode.bgMod(bg) + colorMode.suffix

}
