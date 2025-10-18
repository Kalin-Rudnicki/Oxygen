package oxygen.cli

import oxygen.predef.base.*
import oxygen.predef.color.{*, given}
import scala.language.implicitConversions
import scala.quoted.*

extension (string: String) {
  private def colorizeValue: ColorString = string.magentaFg
  private def colorizeBaseParam: ColorString = string.cyanFg
  private def colorizeTrueParam: ColorString = string.greenFg
  private def colorizeFalseParam: ColorString = string.redFg
}

sealed trait Name {
  lazy val showParam: ColorString
}

sealed trait SimpleName extends Name
sealed trait BooleanName extends Name {
  val trueName: SimpleName
  val falseName: SimpleName
}
sealed trait LongReference extends Name
sealed trait ShortReference extends Name

final case class LongName private (firstChar: ShortName, name: String) extends LongReference with SimpleName {

  lazy val showValue: ColorString = name.colorizeValue
  lazy val showParamRaw: ColorString = name.colorizeBaseParam
  override lazy val showParam: ColorString = color"--${name.colorizeBaseParam}"

  def -(that: LongName): LongName = new LongName(this.firstChar, s"${this.name}-${that.name}")

  override def toString: String = this.name

}
object LongName {

  private[cli] def createInternal(firstChar: ShortName, name: String): LongName = new LongName(firstChar, name)

  given toExpr: ToExpr[LongName] =
    new ToExpr[LongName] {
      override def apply(x: LongName)(using Quotes): Expr[LongName] = '{ LongName.createInternal(${ Expr(x.firstChar) }, ${ Expr(x.name) }) }
    }

  private val regex = "^[A-Za-z0-9]+(?:-[A-Za-z0-9]+)*$".r
  def wrap(name: String): Either[String, LongName] =
    if (regex.matches(name)) new LongName(ShortName.wrap(name.head).fold(e => throw new RuntimeException(e), identity), name).asRight
    else s"Invalid LongName ${name.unesc}".asLeft

  inline def apply(inline name: String): LongName = ${ applyImpl('{ name }) }
  private def applyImpl(name: Expr[String])(using quotes: Quotes): Expr[LongName] = NameMacros.make[String, LongName](name, wrap)

  def unapply(name: String): Some[Either[String, LongName]] = Some(wrap(name))

}

sealed trait BooleanLongName extends LongReference with BooleanName {
  val base: LongName
}
object BooleanLongName {

  final case class PrefixTrue(truePrefix: LongName, base: LongName) extends BooleanLongName {

    override val trueName: LongName = truePrefix - base
    override val falseName: LongName = base

    override lazy val showParam: ColorString = color"--[${truePrefix.name.colorizeTrueParam}-]${base.name.colorizeBaseParam}"

  }

  final case class PrefixFalse(falsePrefix: LongName, base: LongName) extends BooleanLongName {

    override val trueName: LongName = base
    override val falseName: LongName = falsePrefix - base

    override lazy val showParam: ColorString = color"--[${falsePrefix.name.colorizeFalseParam}-]${base.name.colorizeBaseParam}"

  }

  final case class PrefixBoth(truePrefix: LongName, falsePrefix: LongName, base: LongName) extends BooleanLongName {

    override val trueName: LongName = truePrefix - base
    override val falseName: LongName = falsePrefix - base

    override lazy val showParam: ColorString = color"--(${truePrefix.name.colorizeTrueParam}/${falsePrefix.name.colorizeFalseParam})-${base.name.colorizeBaseParam}"

  }

}

sealed abstract class ShortName(final val name: Char) extends ShortReference with SimpleName {

  override final lazy val showParam: ColorString = color"-${name.toString.colorizeBaseParam}"

  final def expand: Either[ShortName.Digit, (ShortName.UpperLetter, ShortName.LowerLetter)] = this match
    case letter: ShortName.LowerLetter => (letter.toUpper, letter).asRight
    case letter: ShortName.UpperLetter => (letter, letter.toLower).asRight
    case digit: ShortName.Digit        => digit.asLeft

  override final def hashCode: Int = this.name.hashCode

  override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: ShortName => this.name == that.name
    case _               => false

  override final def toString: String = this.name.toString

}
object ShortName {

  given toExpr: ToExpr[ShortName] =
    new ToExpr[ShortName] {
      override def apply(x: ShortName)(using Quotes): Expr[ShortName] = x match
        case x: ShortName.LowerLetter => Expr(x)
        case x: ShortName.UpperLetter => Expr(x)
        case x: ShortName.Digit       => Expr(x)
    }

  def wrap(name: Char): Either[String, ShortName] = {
    val n = name.toInt
    if (n >= 'a'.toInt && n <= 'z'.toInt) ShortName.LowerLetter.createInternal(name).asRight
    else if (n >= 'A'.toInt && n <= 'Z'.toInt) ShortName.UpperLetter.createInternal(name).asRight
    else if (n >= '0'.toInt && n <= '9'.toInt) ShortName.Digit.createInternal(name).asRight
    else s"Invalid ShortName ${name.unesc}".asLeft
  }

  inline def apply(inline name: Char): ShortName = ${ applyImpl('{ name }) }
  private def applyImpl(name: Expr[Char])(using quotes: Quotes): Expr[ShortName] = NameMacros.make[Char, ShortName](name, wrap)

  def unapply(name: Char): Some[Either[String, ShortName]] = Some(wrap(name))
  def unapply(name: String): Some[Either[String, ShortName]] = name.toList match
    case name :: Nil => Some(wrap(name))
    case _           => Some(s"Not a single char: ${name.unesc}".asLeft)

  object many {
    def unapply(name: String): Some[Either[String, NonEmptyList[ShortName]]] =
      NonEmptyList.fromList(name.toList) match {
        case Some(chars) => Some(chars.traverse(wrap))
        case None        => Some("Chars are empty".asLeft)
      }
  }

  sealed abstract class Letter(name: Char) extends ShortName(name)

  final class LowerLetter private[ShortName] (name: Char) extends ShortName.Letter(name) {
    def toUpper: UpperLetter = UpperLetter(name.toUpper)
  }
  object LowerLetter {

    private[ShortName] def createInternal(name: Char): LowerLetter = new LowerLetter(name)

    given toExpr: ToExpr[LowerLetter] =
      new ToExpr[LowerLetter] {
        override def apply(x: LowerLetter)(using Quotes): Expr[LowerLetter] = '{ LowerLetter.createInternal(${ Expr(x.name) }) }
      }

  }

  final class UpperLetter private[ShortName] (name: Char) extends ShortName.Letter(name) {
    def toLower: LowerLetter = LowerLetter(name.toLower)
  }
  object UpperLetter {

    private[ShortName] def createInternal(name: Char): UpperLetter = new UpperLetter(name)

    given toExpr: ToExpr[UpperLetter] =
      new ToExpr[UpperLetter] {
        override def apply(x: UpperLetter)(using Quotes): Expr[UpperLetter] = '{ UpperLetter.createInternal(${ Expr(x.name) }) }
      }

  }

  final class Digit private[ShortName] (name: Char) extends ShortName(name)
  object Digit {

    private[ShortName] def createInternal(name: Char): Digit = new Digit(name)

    given toExpr: ToExpr[Digit] =
      new ToExpr[Digit] {
        override def apply(x: Digit)(using Quotes): Expr[Digit] = '{ Digit.createInternal(${ Expr(x.name) }) }
      }

  }

}

final case class BooleanShortName(trueName: ShortName, falseName: ShortName) extends ShortReference with BooleanName {

  override lazy val showParam: ColorString = color"-(${trueName.name.toString.colorizeTrueParam}/${falseName.name.toString.colorizeFalseParam})"

}

private[cli] object NameMacros {

  def make[I, O](i: Expr[I], transform: I => Either[String, O])(using
      quotes: Quotes,
      iFromExpr: FromExpr[I],
      oToExpr: ToExpr[O],
  ): Expr[O] = {
    import quotes.reflect.*

    val iValue: I = i.valueOrAbort

    val oValue: O = transform(iValue) match
      case Right(value) => value
      case Left(error)  => report.errorAndAbort(error)

    Expr(oValue)
  }

}

inline implicit def stringToLongName(inline name: String): LongName = LongName(name)
inline implicit def charToShortName(inline name: Char): ShortName = ShortName(name)
inline implicit def charToDefaultableName(inline name: Char): Defaultable.Some[ShortName] = Defaultable.Some(ShortName(name))
