package oxygen.core

import oxygen.core.syntax.common.*
import oxygen.core.typeclass.*
import scala.quoted.*
import scala.util.Try

sealed trait Color
object Color {

  def apply(r: Int, g: Int, b: Int): Color.RGB =
    RGB(r, g, b)

  object parse {

    def apply(color: String): Option[Color] =
      color.toUpperCase match
        case "DEFAULT"                        => Color.Default.some
        case Named.stringCodec.decoder(named) => named.some
        case Color.RGB.parse(color)           => color.some
        case _                                => None

    def unapply(color: String): Option[Color] =
      Color.parse(color)

  }

  implicit val stringCodec: StringCodec[Color] =
    StringCodec.string.transformOption(Color.parse(_), _.toString)

  sealed trait Concrete extends Color {
    val fgMod: String
    val bgMod: String
    final lazy val fgANSI: String = s"$ANSIEscapeString${fgMod}m"
    final lazy val bgANSI: String = s"$ANSIEscapeString${bgMod}m"
  }

  sealed trait Simple extends Color.Concrete

  sealed abstract class Named(n: Char) extends Color.Simple with Enum[Named] {

    final val lowerName: String = toString.toLowerCase

    override final val fgMod: String = s"3$n"
    override final val bgMod: String = s"4$n"

  }
  object Named extends Enum.Companion[Named] {

    case object Black extends Named('0')
    case object Red extends Named('1')
    case object Green extends Named('2')
    case object Yellow extends Named('3')
    case object Blue extends Named('4')
    case object Magenta extends Named('5')
    case object Cyan extends Named('6')
    case object White extends Named('7')

    override def values: Array[Named] = Array(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White)

  }

  final case class RGB(r: Int, g: Int, b: Int) extends Color.Concrete {

    def toRGBString: String = List(r, g, b).mkString("rgb(", ", ", ")")
    def toHexString: String = List(r, g, b).map(Integer.toString(_, 16).alignRight(2, '0')).mkString("#", "", "")

    override val fgMod: String = s"38;2;$r;$g;$b"
    override val bgMod: String = s"48;2;$r;$g;$b"

    override def toString: String = toHexString

    def :>(simple: Color.Simple): Color.Cased = Color.Cased(this, simple)

  }
  object RGB {

    implicit val stringCodec: StringCodec[Color.RGB] =
      StringCodec.string.transformOption(Color.RGB.parse(_), _.toString)

    private val hexReg3 = "^(?:#|0X|)([0-9A-Z])([0-9A-Z])([0-9A-Z])$".r
    private val hexReg6 = "^(?:#|0X|)([0-9A-Z]{2})([0-9A-Z]{2})([0-9A-Z]{2})$".r
    private val hexInt1: Unapply[String, Int] = s => Try { Integer.parseInt(s"$s$s", 16) }.toOption
    private val hexInt2: Unapply[String, Int] = s => Try { Integer.parseInt(s, 16) }.toOption
    private val rgbInt: Unapply[String, Int] = _.toIntOption.filter(i => i >= 0 && i <= 255)
    private val rgbReg = "^RGB\\([ ]*(\\d+)[ ]*,[ ]*(\\d+)[ ]*,[ ]*(\\d+)[ ]*\\)$".r

    private implicit val rgbToExpr: ToExpr[Color.RGB] =
      new ToExpr[RGB] {
        override def apply(x: Color.RGB)(using quotes: Quotes): Expr[RGB] =
          '{ Color.RGB(${ Expr(x.r) }, ${ Expr(x.g) }, ${ Expr(x.b) }) }
      }

    extension (self: Int)
      private inline def showHex: String =
        Integer.toString(self, 16).alignRight(6, '0')

    object hex {

      object parse {

        def apply(hexStr: String): Option[Color.RGB] =
          hexStr.toUpperCase match
            case hexReg6(hexInt2(r), hexInt2(g), hexInt2(b)) => Color.RGB(r, g, b).some
            case hexReg3(hexInt1(r), hexInt1(g), hexInt1(b)) => Color.RGB(r, g, b).some
            case _                                           => None

        /**
          * @param hexInt Note: this argument is assumed to be in the format of `0xAABBCC`.
          *               If you try to specify in the format `0xABC`, it will be interpreted as `0x000ABC` not `0xAABBCC`.
          *               The string version of this function will handle that correctly, the int version will not.
          */
        def apply(hexInt: Int): Option[Color.RGB] =
          Color.RGB.hex.parse(hexInt.showHex)

        def unapply(hexStr: String): Option[Color.RGB] = Color.RGB.hex.parse(hexStr)

      }

      inline def apply(inline hexStr: String): Color.RGB = ${ strMacroImpl('hexStr) }

      /**
        * @param hexInt Note: this argument is assumed to be in the format of `0xAABBCC`.
        *               If you try to specify in the format `0xABC`, it will be interpreted as `0x000ABC` not `0xAABBCC`.
        *               The string version of this function will handle that correctly, the int version will not.
        */
      inline def apply(inline hexInt: Int): Color.RGB = ${ intMacroImpl('hexInt) }

      private def strMacroImpl(hexExpr: Expr[String])(using quotes: Quotes): Expr[Color.RGB] = {
        import quotes.reflect.*
        val hexStr = hexExpr.valueOrAbort
        Color.RGB.hex.parse(hexStr) match
          case Some(color) => Expr(color)
          case None        => report.errorAndAbort(s"Malformed hex color: $hexStr")
      }

      private def intMacroImpl(hexExpr: Expr[Int])(using quotes: Quotes): Expr[Color.RGB] = {
        import quotes.reflect.*
        val hexInt = hexExpr.valueOrAbort
        Color.RGB.hex.parse(hexInt) match
          case Some(color) => Expr(color)
          case None        => report.errorAndAbort(s"Malformed hex int: 0x${hexInt.showHex}")
      }

    }

    object rgb {

      object parse {

        def apply(rgbStr: String): Option[Color.RGB] =
          rgbStr.toUpperCase match
            case rgbReg(rgbInt(r), rgbInt(g), rgbInt(b)) => Color.RGB(r, g, b).some
            case _                                       => None

        def unapply(rgbStr: String): Option[Color.RGB] = Color.RGB.rgb.parse(rgbStr)

      }

      inline def apply(inline hexStr: String): Color.RGB = ${ macroImpl('hexStr) }

      private def macroImpl(rgbExpr: Expr[String])(using quotes: Quotes): Expr[Color.RGB] = {
        import quotes.reflect.*
        val rgbStr = rgbExpr.valueOrAbort
        Color.RGB.rgb.parse(rgbStr) match
          case Some(color) => Expr(color)
          case None        => report.errorAndAbort(s"Malformed rgb color: $rgbStr")
      }

    }

    object parse {

      def apply(hexOrRgbStr: String): Option[Color.RGB] =
        Color.RGB.rgb.parse(hexOrRgbStr).orElse(Color.RGB.rgb.parse(hexOrRgbStr))

      def unapply(hexOrRgbStr: String): Option[Color.RGB] = Color.RGB.parse(hexOrRgbStr)

    }

    inline def apply(inline hexOrRgbStr: String): Color.RGB = ${ macroImpl('hexOrRgbStr) }

    private def macroImpl(hexOrRgbExpr: Expr[String])(using quotes: Quotes): Expr[Color.RGB] = {
      import quotes.reflect.*
      val hexOrRgbStr = hexOrRgbExpr.valueOrAbort
      Color.RGB.parse(hexOrRgbStr) match
        case Some(color) => Expr(color)
        case None        => report.errorAndAbort(s"Malformed hex/rgb color: $hexOrRgbStr")
    }

  }

  case object Default extends Color.Simple {
    override val fgMod: String = "39"
    override val bgMod: String = "49"
  }

  final case class Cased(extended: Color.RGB, simple: Color.Simple) extends Color

}
