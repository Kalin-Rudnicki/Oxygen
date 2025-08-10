package oxygen.quoted.companion

import oxygen.quoted.*
import scala.annotation.unused
import scala.quoted.*

final class ConstantCompanion(using @unused quotes: Quotes) {}

final class BooleanConstantCompanion(using quotes: Quotes) {

  /** Create a constant Boolean value */
  def apply(x: Boolean): BooleanConstant =
    BooleanConstant.wrap(quotes.reflect.BooleanConstant.apply(x))

}

final class ByteConstantCompanion(using quotes: Quotes) {

  /** Create a constant Byte value */
  def apply(x: Byte): ByteConstant =
    ByteConstant.wrap(quotes.reflect.ByteConstant.apply(x))

}

final class ShortConstantCompanion(using quotes: Quotes) {

  /** Create a constant Short value */
  def apply(x: Short): ShortConstant =
    ShortConstant.wrap(quotes.reflect.ShortConstant.apply(x))

}

final class IntConstantCompanion(using quotes: Quotes) {

  /** Create a constant Int value */
  def apply(x: Int): IntConstant =
    IntConstant.wrap(quotes.reflect.IntConstant.apply(x))

}

final class LongConstantCompanion(using quotes: Quotes) {

  /** Create a constant Long value */
  def apply(x: Long): LongConstant =
    LongConstant.wrap(quotes.reflect.LongConstant.apply(x))

}

final class FloatConstantCompanion(using quotes: Quotes) {

  /** Create a constant Float value */
  def apply(x: Float): FloatConstant =
    FloatConstant.wrap(quotes.reflect.FloatConstant.apply(x))

}

final class DoubleConstantCompanion(using quotes: Quotes) {

  /** Create a constant Double value */
  def apply(x: Double): DoubleConstant =
    DoubleConstant.wrap(quotes.reflect.DoubleConstant.apply(x))

}

final class CharConstantCompanion(using quotes: Quotes) {

  /** Create a constant Char value */
  def apply(x: Char): CharConstant =
    CharConstant.wrap(quotes.reflect.CharConstant.apply(x))

}

final class StringConstantCompanion(using quotes: Quotes) {

  /** Create a constant String value */
  def apply(x: String): StringConstant =
    StringConstant.wrap(quotes.reflect.StringConstant.apply(x))

}

final class UnitConstantCompanion(using quotes: Quotes) {

  /** Create a constant Unit value */
  def apply(): UnitConstant =
    UnitConstant.wrap(quotes.reflect.UnitConstant.apply())

}

final class NullConstantCompanion(using quotes: Quotes) {

  /** Create a constant null value */
  def apply(): NullConstant =
    NullConstant.wrap(quotes.reflect.NullConstant.apply())

}

final class ClassOfConstantCompanion(using quotes: Quotes) {

  /** Create a constant class value representing `classOf[<tpe>]` */
  def apply(tpe: TypeRepr): ClassOfConstant =
    ClassOfConstant.wrap(quotes.reflect.ClassOfConstant.apply(tpe.unwrapWithin))

}
