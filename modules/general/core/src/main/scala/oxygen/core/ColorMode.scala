package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.option.*
import oxygen.core.syntax.string.*

sealed trait ColorMode extends Enum[ColorMode] {
  def toConcrete(color: Color): Option[Color.Concrete]
}
object ColorMode extends Enum.Companion[ColorMode] {

  // =====|  |=====

  sealed trait NonColorless extends ColorMode {
    val prefix: String
    val suffix: String
    def fgMod(color: Color.Concrete): String
    def bgMod(color: Color.Concrete): String
  }

  sealed trait Colored extends NonColorless {
    override final val prefix: String = ANSIEscapeString
    override final val suffix: String = "m"
    override final def fgMod(color: Color.Concrete): String = color.fgMod
    override final def bgMod(color: Color.Concrete): String = color.bgMod
  }

  // =====|  |=====

  case object Extended extends Colored {
    override def toConcrete(color: Color): Option[Color.Concrete] = color match
      case color: Color.Concrete    => color.some
      case Color.Cased(extended, _) => extended.some
  }

  case object Simple extends Colored {
    override def toConcrete(color: Color): Option[Color.Concrete] = color match
      case color: Color.Simple    => color.some
      case _: Color.RGB           => None
      case Color.Cased(_, simple) => simple.some
  }

  case object PreferSimple extends Colored {
    override def toConcrete(color: Color): Option[Color.Concrete] = color match
      case color: Color.Concrete  => color.some
      case Color.Cased(_, simple) => simple.some
  }

  case object ShowColorName extends NonColorless {

    override def toConcrete(color: Color): Option[Color.Concrete] = Extended.toConcrete(color)

    override val prefix: String = "[["
    override val suffix: String = "]]"

    private def showColor(color: Color.Concrete): String = color match
      case Color.Default      => "default"
      case color: Color.Named => color.lowerName
      case color: Color.RGB   => color.toRGBString

    override def fgMod(color: Color.Concrete): String = s"fg:${showColor(color)}"
    override def bgMod(color: Color.Concrete): String = s"bg:${showColor(color)}"

  }

  case object Colorless extends ColorMode {
    override def toConcrete(color: Color): Option[Color.Concrete] = None
  }

  // =====|  |=====

  override protected val defaultToString: ColorMode => NonEmptyList[String] = e => NonEmptyList.one(e.toString.camelToSnake.snakeToDash)

  override def values: Array[ColorMode] = Array(Extended, Simple, Colorless, ShowColorName)

}
