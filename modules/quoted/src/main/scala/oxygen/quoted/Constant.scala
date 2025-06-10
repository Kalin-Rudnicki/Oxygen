package oxygen.quoted

import oxygen.quoted.companion.*
import oxygen.quoted.error.UnknownCase
import scala.quoted.*

sealed trait Constant extends Model {
  type This <: Constant
  val quotes: Quotes
  val unwrap: quotes.reflect.Constant
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Constant = unwrap.asInstanceOf[newQuotes.reflect.Constant]
  given givenQuotes: quotes.type = quotes

  /** Returns the value of the constant */
  def value: Any

  /** Shows the constant as a String */
  def show(using printer: Printer[Constant]): String = printer.show(this)

}
object Constant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Constant): Constant = unwrap match
    case b: quotes.reflect.BooleanConstant  => BooleanConstant.wrap(b)
    case b: quotes.reflect.ByteConstant     => ByteConstant.wrap(b)
    case s: quotes.reflect.ShortConstant    => ShortConstant.wrap(s)
    case i: quotes.reflect.IntConstant      => IntConstant.wrap(i)
    case l: quotes.reflect.LongConstant     => LongConstant.wrap(l)
    case f: quotes.reflect.FloatConstant    => FloatConstant.wrap(f)
    case d: quotes.reflect.DoubleConstant   => DoubleConstant.wrap(d)
    case c: quotes.reflect.CharConstant     => CharConstant.wrap(c)
    case s: quotes.reflect.StringConstant   => StringConstant.wrap(s)
    case u: quotes.reflect.UnitConstant     => UnitConstant.wrap(u)
    case n: quotes.reflect.NullConstant     => NullConstant.wrap(n)
    case cl: quotes.reflect.ClassOfConstant => ClassOfConstant.wrap(cl)
    case _                                  => throw UnknownCase("Constant", unwrap)

  def companion(using quotes: Quotes): ConstantCompanion = ConstantCompanion(using quotes)
  given Quotes => Conversion[Constant.type, ConstantCompanion] = _.companion

}

final class BooleanConstant(val quotes: Quotes)(val unwrap: quotes.reflect.BooleanConstant) extends Constant {
  override type This <: BooleanConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.BooleanConstant = unwrap.asInstanceOf[newQuotes.reflect.BooleanConstant]

  override def value: Boolean = this.unwrap.value.asInstanceOf[Boolean]

}
object BooleanConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.BooleanConstant): BooleanConstant =
    new BooleanConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): BooleanConstantCompanion = BooleanConstantCompanion(using quotes)
  given Quotes => Conversion[BooleanConstant.type, BooleanConstantCompanion] = _.companion

  /** Match Boolean value constant and extract its value */
  def unapply(constant: BooleanConstant): Some[Boolean] = {
    given q: Quotes = constant.quotes
    q.reflect.BooleanConstant.unapply(constant.unwrapWithin)
  }

}

final class ByteConstant(val quotes: Quotes)(val unwrap: quotes.reflect.ByteConstant) extends Constant {
  override type This <: ByteConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ByteConstant = unwrap.asInstanceOf[newQuotes.reflect.ByteConstant]

  override def value: Byte = this.unwrap.value.asInstanceOf[Byte]

}
object ByteConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ByteConstant): ByteConstant =
    new ByteConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): ByteConstantCompanion = ByteConstantCompanion(using quotes)
  given Quotes => Conversion[ByteConstant.type, ByteConstantCompanion] = _.companion

  /** Match Byte value constant and extract its value */
  def unapply(constant: ByteConstant): Some[Byte] = {
    given q: Quotes = constant.quotes
    q.reflect.ByteConstant.unapply(constant.unwrapWithin)
  }

}

final class ShortConstant(val quotes: Quotes)(val unwrap: quotes.reflect.ShortConstant) extends Constant {
  override type This <: ShortConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ShortConstant = unwrap.asInstanceOf[newQuotes.reflect.ShortConstant]

  override def value: Short = this.unwrap.value.asInstanceOf[Short]

}
object ShortConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ShortConstant): ShortConstant =
    new ShortConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): ShortConstantCompanion = ShortConstantCompanion(using quotes)
  given Quotes => Conversion[ShortConstant.type, ShortConstantCompanion] = _.companion

  /** Match Short value constant and extract its value */
  def unapply(constant: ShortConstant): Some[Short] = {
    given q: Quotes = constant.quotes
    q.reflect.ShortConstant.unapply(constant.unwrapWithin)
  }

}

final class IntConstant(val quotes: Quotes)(val unwrap: quotes.reflect.IntConstant) extends Constant {
  override type This <: IntConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.IntConstant = unwrap.asInstanceOf[newQuotes.reflect.IntConstant]

  override def value: Int = this.unwrap.value.asInstanceOf[Int]

}
object IntConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.IntConstant): IntConstant =
    new IntConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): IntConstantCompanion = IntConstantCompanion(using quotes)
  given Quotes => Conversion[IntConstant.type, IntConstantCompanion] = _.companion

  /** Match Int value constant and extract its value */
  def unapply(constant: IntConstant): Some[Int] = {
    given q: Quotes = constant.quotes
    q.reflect.IntConstant.unapply(constant.unwrapWithin)
  }

}

final class LongConstant(val quotes: Quotes)(val unwrap: quotes.reflect.LongConstant) extends Constant {
  override type This <: LongConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.LongConstant = unwrap.asInstanceOf[newQuotes.reflect.LongConstant]

  override def value: Long = this.unwrap.value.asInstanceOf[Long]

}
object LongConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.LongConstant): LongConstant =
    new LongConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): LongConstantCompanion = LongConstantCompanion(using quotes)
  given Quotes => Conversion[LongConstant.type, LongConstantCompanion] = _.companion

  /** Match Long value constant and extract its value */
  def unapply(constant: LongConstant): Some[Long] = {
    given q: Quotes = constant.quotes
    q.reflect.LongConstant.unapply(constant.unwrapWithin)
  }

}

final class FloatConstant(val quotes: Quotes)(val unwrap: quotes.reflect.FloatConstant) extends Constant {
  override type This <: FloatConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.FloatConstant = unwrap.asInstanceOf[newQuotes.reflect.FloatConstant]

  override def value: Float = this.unwrap.value.asInstanceOf[Float]

}
object FloatConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.FloatConstant): FloatConstant =
    new FloatConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): FloatConstantCompanion = FloatConstantCompanion(using quotes)
  given Quotes => Conversion[FloatConstant.type, FloatConstantCompanion] = _.companion

  /** Match Float value constant and extract its value */
  def unapply(constant: FloatConstant): Some[Float] = {
    given q: Quotes = constant.quotes
    q.reflect.FloatConstant.unapply(constant.unwrapWithin)
  }

}

final class DoubleConstant(val quotes: Quotes)(val unwrap: quotes.reflect.DoubleConstant) extends Constant {
  override type This <: DoubleConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.DoubleConstant = unwrap.asInstanceOf[newQuotes.reflect.DoubleConstant]

  override def value: Double = this.unwrap.value.asInstanceOf[Double]

}
object DoubleConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.DoubleConstant): DoubleConstant =
    new DoubleConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): DoubleConstantCompanion = DoubleConstantCompanion(using quotes)
  given Quotes => Conversion[DoubleConstant.type, DoubleConstantCompanion] = _.companion

  /** Match Double value constant and extract its value */
  def unapply(constant: DoubleConstant): Some[Double] = {
    given q: Quotes = constant.quotes
    q.reflect.DoubleConstant.unapply(constant.unwrapWithin)
  }

}

final class CharConstant(val quotes: Quotes)(val unwrap: quotes.reflect.CharConstant) extends Constant {
  override type This <: CharConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.CharConstant = unwrap.asInstanceOf[newQuotes.reflect.CharConstant]

  override def value: Char = this.unwrap.value.asInstanceOf[Char]

}
object CharConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.CharConstant): CharConstant =
    new CharConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): CharConstantCompanion = CharConstantCompanion(using quotes)
  given Quotes => Conversion[CharConstant.type, CharConstantCompanion] = _.companion

  /** Match Char value constant and extract its value */
  def unapply(constant: CharConstant): Some[Char] = {
    given q: Quotes = constant.quotes
    q.reflect.CharConstant.unapply(constant.unwrapWithin)
  }

}

final class StringConstant(val quotes: Quotes)(val unwrap: quotes.reflect.StringConstant) extends Constant {
  override type This <: StringConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.StringConstant = unwrap.asInstanceOf[newQuotes.reflect.StringConstant]

  override def value: String = this.unwrap.value.asInstanceOf[String]

}
object StringConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.StringConstant): StringConstant =
    new StringConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): StringConstantCompanion = StringConstantCompanion(using quotes)
  given Quotes => Conversion[StringConstant.type, StringConstantCompanion] = _.companion

  /** Match String value constant and extract its value */
  def unapply(constant: StringConstant): Some[String] = {
    given q: Quotes = constant.quotes
    q.reflect.StringConstant.unapply(constant.unwrapWithin)
  }

}

final class UnitConstant(val quotes: Quotes)(val unwrap: quotes.reflect.UnitConstant) extends Constant {
  override type This <: UnitConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.UnitConstant = unwrap.asInstanceOf[newQuotes.reflect.UnitConstant]

  override def value: Unit = this.unwrap.value.asInstanceOf[Unit]

}
object UnitConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.UnitConstant): UnitConstant =
    new UnitConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): UnitConstantCompanion = UnitConstantCompanion(using quotes)
  given Quotes => Conversion[UnitConstant.type, UnitConstantCompanion] = _.companion

  /** Match Unit value constant */
  def unapply(constant: UnitConstant): Boolean = {
    given q: Quotes = constant.quotes
    q.reflect.UnitConstant.unapply(constant.unwrapWithin)
  }

}

final class NullConstant(val quotes: Quotes)(val unwrap: quotes.reflect.NullConstant) extends Constant {
  override type This <: NullConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.NullConstant = unwrap.asInstanceOf[newQuotes.reflect.NullConstant]

  override def value: Null = this.unwrap.value.asInstanceOf[Null]

}
object NullConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.NullConstant): NullConstant =
    new NullConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): NullConstantCompanion = NullConstantCompanion(using quotes)
  given Quotes => Conversion[NullConstant.type, NullConstantCompanion] = _.companion

  def unapply(constant: NullConstant): Boolean =
    constant.quotes.reflect.NullConstant.unapply(constant.unwrap)

}

final class ClassOfConstant(val quotes: Quotes)(val unwrap: quotes.reflect.ClassOfConstant) extends Constant {
  override type This <: ClassOfConstant
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ClassOfConstant = unwrap.asInstanceOf[newQuotes.reflect.ClassOfConstant]

  // TODO (KR) : can be more specific? returns a Class[?]?
  override def value: Any = this.unwrap.value

}
object ClassOfConstant {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ClassOfConstant): ClassOfConstant =
    new ClassOfConstant(quotes)(unwrap)

  def companion(using quotes: Quotes): ClassOfConstantCompanion = ClassOfConstantCompanion(using quotes)
  given Quotes => Conversion[ClassOfConstant.type, ClassOfConstantCompanion] = _.companion

  def unapply(constant: ClassOfConstant): Option[TypeRepr] =
    constant.quotes.reflect.ClassOfConstant.unapply(constant.unwrap).map(TypeRepr.wrap(using constant.quotes)(_))

}
