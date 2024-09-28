package oxygen.core

import oxygen.core.syntax.common.*
import oxygen.core.syntax.seq.*
import scala.collection.mutable
import scala.util.matching.Regex

final case class ColorString(color: ColorState, elems: Seq[String | ColorString]) {

  // =====|  |=====

  def +(that: ColorString): ColorString =
    if (this.color == that.color) ColorString(this.color, this.elems ++ that.elems)
    else if (this.isEmpty) that
    else if (that.isEmpty) this
    else ColorString(ColorState.empty, this :: that :: Nil)

  def isEmpty: Boolean = elems.isEmpty
  def nonEmpty: Boolean = elems.nonEmpty

  def detailedSplit(regex: Regex): (Boolean, Seq[ColorString], Boolean) =
    if (elems.isEmpty) (false, Seq.empty, false)
    else {
      val childSplits: Seq[(Boolean, Seq[String | ColorString], Boolean)] =
        elems.map {
          case string: String           => string.detailedSplit(regex)
          case colorString: ColorString => colorString.detailedSplit(regex)
        }
      val (nestedChildElems: Seq[Seq[String | ColorString]], endedOnEmptySplit: Boolean) =
        childSplits.mapPassLeft(false) { case (prev, (a, strs, b)) =>
          (b, if (prev && a) "" +: strs else strs)
        }

      (
        childSplits.head._1,
        nestedChildElems.flatten[String | ColorString].map(s => ColorString(this.color, Seq(s))),
        endedOnEmptySplit,
      )
    }

  def split(regex: Regex, includeEmptyLeading: Boolean, includeEmptyTrailing: Boolean): Seq[ColorString] = {
    val (hasEmptyLeading, strs, hasEmptyTrailing) = detailedSplit(regex)
    val tmp = if (hasEmptyLeading && includeEmptyLeading) ColorString.make("") +: strs else strs
    if (hasEmptyTrailing && includeEmptyTrailing) tmp :+ ColorString.make("") else tmp
  }
  def split(regex: Regex): Seq[ColorString] =
    split(regex, true, true)

  // =====| Modification |=====

  inline def withColor(color: ColorState): ColorString = copy(color = color)

  // --- FG ---

  inline def withFg(fg: Option[Color]): ColorString = copy(color.copy(fg = fg))
  inline def withFg(fg: Color): ColorString = copy(color.copy(fg = fg.some))
  inline def withoutFg: ColorString = copy(color.copy(fg = None))

  def blackFg: ColorString = withFg(Color.Named.Black)
  def redFg: ColorString = withFg(Color.Named.Red)
  def greenFg: ColorString = withFg(Color.Named.Green)
  def yellowFg: ColorString = withFg(Color.Named.Yellow)
  def blueFg: ColorString = withFg(Color.Named.Blue)
  def magentaFg: ColorString = withFg(Color.Named.Magenta)
  def cyanFg: ColorString = withFg(Color.Named.Cyan)
  def whiteFg: ColorString = withFg(Color.Named.White)

  def rgbFg(r: Int, g: Int, b: Int): ColorString = withFg(Color.RGB(r, g, b))
  def rgbFg(r: Int, g: Int, b: Int, backup: Color.Simple): ColorString = withFg(Color.RGB(r, g, b) :> backup)
  inline def hexFg(inline hexStr: String): ColorString = withFg(Color.RGB.hex(hexStr))
  inline def hexFg(inline hexStr: String, backup: Color.Simple): ColorString = withFg(Color.RGB.hex(hexStr) :> backup)

  def resetFg: ColorString = withFg(Color.Default)

  // --- BG ---

  inline def withBg(bg: Option[Color]): ColorString = copy(color.copy(bg = bg))
  inline def withBg(bg: Color): ColorString = copy(color.copy(bg = bg.some))
  inline def withoutBg: ColorString = copy(color.copy(bg = None))

  def blackBg: ColorString = withBg(Color.Named.Black)
  def redBg: ColorString = withBg(Color.Named.Red)
  def greenBg: ColorString = withBg(Color.Named.Green)
  def yellowBg: ColorString = withBg(Color.Named.Yellow)
  def blueBg: ColorString = withBg(Color.Named.Blue)
  def magentaBg: ColorString = withBg(Color.Named.Magenta)
  def cyanBg: ColorString = withBg(Color.Named.Cyan)
  def whiteBg: ColorString = withBg(Color.Named.White)

  def rgbBg(r: Int, g: Int, b: Int): ColorString = withBg(Color.RGB(r, g, b))
  def rgbBg(r: Int, g: Int, b: Int, backup: Color.Simple): ColorString = withBg(Color.RGB(r, g, b) :> backup)
  inline def hexBg(inline hexStr: String): ColorString = withBg(Color.RGB.hex(hexStr))
  inline def hexBg(inline hexStr: String, backup: Color.Simple): ColorString = withBg(Color.RGB.hex(hexStr) :> backup)

  def resetBg: ColorString = withBg(Color.Default)

  // =====| Show |=====

  private def writeToStringBuilderNoColor(builder: mutable.StringBuilder): Unit =
    this.elems.foreach {
      case string: String           => builder.append(string)
      case colorString: ColorString => colorString.writeToStringBuilderNoColor(builder)
    }

  def rawString: String = {
    val builder = mutable.StringBuilder()
    writeToStringBuilderNoColor(builder)
    builder.toString()
  }

  private def writeToStringBuilder(builder: mutable.StringBuilder, outerState: ColorState.Concrete): Unit = {
    val (open, close, newState) = outerState.diff(this.color)

    builder.append(open)

    this.elems.foreach {
      case string: String           => builder.append(string)
      case colorString: ColorString => colorString.writeToStringBuilder(builder, newState)
    }

    builder.append(close)
  }

  def toString(outerColorState: ColorState, colorMode: ColorMode): String =
    colorMode match {
      case colorMode: ColorMode.NonColorless =>
        val builder = mutable.StringBuilder()
        writeToStringBuilder(builder, ColorState.Concrete(colorMode, outerColorState))
        builder.toString()
      case ColorMode.Colorless =>
        rawString
    }
  def toString(outerFgColor: Color): String =
    toString(ColorState.fg(outerFgColor), ColorMode.Extended)
  def toString(colorMode: ColorMode): String =
    toString(ColorState.empty, colorMode)
  override def toString: String =
    toString(ColorState.empty, ColorMode.Extended)

}
object ColorString {

  def make(color: ColorState)(elems: (String | ColorString)*): ColorString = ColorString(color, elems)
  def make(elems: (String | ColorString)*): ColorString = ColorString(ColorState.empty, elems)

  def fromAny(any: Any): ColorString = any.asInstanceOf[Matchable] match
    case colorString: ColorString => colorString
    case string: String           => ColorString.make(string)
    case _                        => ColorString.make(any.toString)

  val empty: ColorString = ColorString(ColorState.empty, Nil)

}

given convertStringToColorString: Conversion[String, ColorString] = str => ColorString(ColorState.empty, str :: Nil)

implicit class ColorStringInterpolator(sc: StringContext) {

  def color(args: ColorString*): ColorString = {
    val builder = IndexedSeq.newBuilder[String | ColorString]

    sc.parts.zipAll(args, "", ColorString.empty).foreach { case (string, colorString) =>
      if (string.length > 0) // some weird bug where `"".nonEmpty` was returning `true`...
        builder.addOne(string)
      if (colorString.nonEmpty)
        builder.addOne(colorString)
    }

    ColorString(ColorState.empty, builder.result())
  }

}

extension (self: Seq[ColorString]) {

  def csMkString: ColorString = ColorString(ColorState.empty, self)

  def csMkString(sep: String | ColorString): ColorString =
    if (self.isEmpty) ColorString.empty
    else ColorString(ColorState.empty, self.head +: self.tail.flatMap(Seq(sep, _)))

  def csMkString(start: String | ColorString, sep: String | ColorString, end: String | ColorString): ColorString =
    if (self.isEmpty) ColorString(ColorState.empty, Seq(start, end))
    else ColorString(ColorState.empty, start +: self.head +: self.tail.flatMap(Seq(sep, _)) :+ end)

}
