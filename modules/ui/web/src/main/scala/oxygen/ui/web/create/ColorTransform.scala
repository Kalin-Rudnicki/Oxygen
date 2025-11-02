package oxygen.ui.web.create

import oxygen.predef.core.*

sealed trait ColorTransform derives Show {

  def transform(color: CSSColor): CSSColor

  final def transform(color: String): String = {
    val tmp = CSSVar.eval(color)
    CSSColor.parse(tmp) match
      case Some(color) => transform(color).show
      case None        => tmp
  }

  final def >>>(that: ColorTransform): ColorTransform = ColorTransform.AndThen(this, that)
  final def <<<(that: ColorTransform): ColorTransform = ColorTransform.AndThen(that, this)

}
object ColorTransform {

  def darken(percent: Double): ColorTransform = Darken(percent)
  def lighten(percent: Double): ColorTransform = Lighten(percent)
  def setOpacity(percent: Double): ColorTransform = SetOpacity(percent)
  def mix(c2: CSSColor): ColorTransform = Mix(c2, 1, 1)
  def mix(c1W: Int)(c2: CSSColor, c2W: Int): ColorTransform = Mix(c2, c1W, c2W)
  def const(c2: CSSColor): ColorTransform = Const(c2)
  def const(c2: CSSVar): ColorTransform = Const(c2.getColorValue)
  def const(c2: String): ColorTransform = Const(CSSColor(c2))
  def none: ColorTransform = NoTransform
  def transparent: ColorTransform = const(CSSColor.transparent)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private sealed abstract class Simple(f: CSSColor => CSSColor) extends ColorTransform {
    override def transform(color: CSSColor): CSSColor = f(color)
  }

  private final case class Darken(percent: Double) extends Simple(_.darken(percent))
  private final case class Lighten(percent: Double) extends Simple(_.lighten(percent))
  private final case class SetOpacity(percent: Double) extends Simple(_.setOpacity(percent))
  private final case class Mix(c2: CSSColor, c1W: Int, c2W: Int) extends Simple(_.mix(c1W)(c2, c2W))
  private final case class Const(c2: CSSColor) extends Simple(_ => c2)

  private case object NoTransform extends ColorTransform {
    override def transform(color: CSSColor): CSSColor = color
  }

  private final case class AndThen(a: ColorTransform, b: ColorTransform) extends ColorTransform {
    override def transform(color: CSSColor): CSSColor = b.transform(a.transform(color))
  }

}
