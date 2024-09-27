package oxygen.core

import oxygen.core.syntax.common.*
import oxygen.core.syntax.seq.*
import scala.collection.mutable
import scala.util.matching.Regex

final case class ColorString(color: ColorString.FgBgColor, elems: Seq[String | ColorString]) {

  // =====|  |=====

  def +(that: ColorString): ColorString =
    if (this.color == that.color) ColorString(this.color, this.elems ++ that.elems)
    else if (this.isEmpty) that
    else if (that.isEmpty) this
    else ColorString(ColorString.FgBgColor.empty, this :: that :: Nil)

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

  private def optimize(outerColor: ColorString.FgBgColor): Seq[String | ColorString] = {
    val newColor = outerColor.overwrite(this.color)
    val newElems: Seq[String | ColorString] =
      this.elems
        .flatMap {
          case string: String           => Seq(string)
          case colorString: ColorString => colorString.optimize(newColor)
        }
        .filter {
          case string: String           => string.nonEmpty
          case colorString: ColorString => colorString.nonEmpty
        }

    if (newElems.isEmpty) Seq.empty
    else if (newColor == outerColor) newElems
    else {
      val (last, acc) = newElems.tail.foldLeft((newElems.head, List.empty[String | ColorString])) {
        case ((last: String, acc), next: String)                                       => (last + next, acc)
        case ((last: ColorString, acc), next: ColorString) if last.color == next.color => (ColorString(last.color, last.elems ++ next.elems), acc)
        case ((last, acc), next)                                                       => (next, last :: acc)
      }

      Seq(
        ColorString(
          this.color,
          (last :: acc).toIndexedSeq.reverse,
        ),
      )
    }
  }

  def optimize: ColorString = ColorString(this.color, this.optimize(ColorString.FgBgColor.empty))

  // =====| Modification |=====

  inline def withColor(color: ColorString.FgBgColor): ColorString = copy(color = color)

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

  private def writeToStringBuilder(builder: mutable.StringBuilder, outerState: ColorString.FgBgColor): Unit = {
    val (open, close, newState) = outerState.diff(this.color)

    builder.append(open)

    this.elems.foreach {
      case string: String           => builder.append(string)
      case colorString: ColorString => colorString.writeToStringBuilder(builder, newState)
    }

    builder.append(close)
  }

  def toString(outerColorState: ColorString.FgBgColor): String = {
    val builder = mutable.StringBuilder()
    writeToStringBuilder(builder, outerColorState)
    builder.toString()
  }
  def toString(outerFgColor: Color): String =
    toString(ColorString.FgBgColor(outerFgColor.some, None))
  override def toString: String =
    toString(ColorString.FgBgColor.empty)

}
object ColorString {

  def make(color: FgBgColor)(elems: (String | ColorString)*): ColorString = ColorString(color, elems)
  def make(elems: (String | ColorString)*): ColorString = ColorString(FgBgColor.empty, elems)

  final case class FgBgColor(fg: Option[Color], bg: Option[Color]) { outer =>

    def overwrite(inner: FgBgColor): FgBgColor = FgBgColor(inner.fg.orElse(outer.fg), inner.bg.orElse(outer.bg))
    def underwrite(inner: FgBgColor): FgBgColor = FgBgColor(outer.fg.orElse(inner.fg), outer.bg.orElse(inner.bg))

    def isUnaffectedBy(inner: FgBgColor): Boolean = outer.overwrite(inner) == outer

    def diff(inner: FgBgColor): (String, String, FgBgColor) = {
      def newColor(outer: Option[Color], inner: Option[Color], mod: Color => String): (Option[String], Option[String]) =
        (outer, inner) match
          case (None, Some(inner))                          => (mod(inner).some, mod(Color.Default).some)
          case (Some(outer), Some(inner)) if outer != inner => (mod(inner).some, mod(outer).some)
          case _                                            => (None, None)

      def joinMods(mods: Option[String]*): String = {
        val flat = mods.flatten
        if (flat.isEmpty) ""
        else s"$ANSIEscapeString${flat.mkString(";")}m"
      }

      val (a, b) = newColor(outer.fg, inner.fg, _.fgMod)
      val (c, d) = newColor(outer.bg, inner.bg, _.bgMod)
      (
        joinMods(a, c),
        joinMods(b, d),
        outer.overwrite(inner),
      )
    }

  }
  object FgBgColor {

    val empty: FgBgColor = FgBgColor(None, None)

  }

  val empty: ColorString = ColorString(FgBgColor.empty, Nil)

}

given convertStringToColorString: Conversion[String, ColorString] = str => ColorString(ColorString.FgBgColor(None, None), str :: Nil)

implicit class ColorStringInterpolator(sc: StringContext) {

  def color(args: ColorString*): ColorString = {
    val builder = IndexedSeq.newBuilder[String | ColorString]

    sc.parts.zipAll(args, "", ColorString.empty).foreach { case (string, colorString) =>
      if (string.length > 0) // some weird bug where `"".nonEmpty` was returning `true`...
        builder.addOne(string)
      if (colorString.nonEmpty)
        builder.addOne(colorString)
    }

    ColorString(
      ColorString.FgBgColor(None, None),
      builder.result(),
    )
  }

}

extension (self: Seq[ColorString]) {

  def csMkString: ColorString = ColorString(ColorString.FgBgColor.empty, self)

  def csMkString(sep: String | ColorString): ColorString =
    if (self.isEmpty) ColorString.empty
    else ColorString(ColorString.FgBgColor.empty, self.head +: self.tail.flatMap(Seq(sep, _)))

  def csMkString(start: String | ColorString, sep: String | ColorString, end: String | ColorString): ColorString =
    if (self.isEmpty) ColorString(ColorString.FgBgColor.empty, Seq(start, end))
    else ColorString(ColorString.FgBgColor.empty, start +: self.head +: self.tail.flatMap(Seq(sep, _)) :+ end)

}
