package oxygen.core.model.currency

import oxygen.core.{str, Text}
import oxygen.core.syntax.common.*
import oxygen.core.typeclass.Showable

final case class UnpreciseMoney(value: BigDecimal, currencyCode: CurrencyCode) extends Showable {

  def precise(roundMode: RoundMode = RoundMode.Error): PreciseMoney = PreciseMoney.fromUnprecise(this, roundMode)

  def unary_- : UnpreciseMoney = UnpreciseMoney(-value, currencyCode)

  def add(by: MoneyValue): UnpreciseMoney = UnpreciseMoney(value + UnpreciseMoney(by, currencyCode).value, currencyCode)
  def subtract(by: MoneyValue): UnpreciseMoney = UnpreciseMoney(value - UnpreciseMoney(by, currencyCode).value, currencyCode)

  def times(by: BigDecimal): UnpreciseMoney = UnpreciseMoney(value * by, currencyCode)
  def times(by: Double): UnpreciseMoney = times(BigDecimal(by))
  def times(by: Long): UnpreciseMoney = times(BigDecimal(by))

  def divide(by: BigDecimal): UnpreciseMoney = UnpreciseMoney(value / by, currencyCode)
  def divide(by: Double): UnpreciseMoney = divide(BigDecimal(by))
  def divide(by: Long): UnpreciseMoney = divide(BigDecimal(by))

  def show(includeCurrency: Boolean, forceDecimal: Boolean): Text = {
    val (whole, tmpPart): (String, Option[String]) =
      value.toString.split('.').toList match
        case whole :: "0" :: Nil  => (whole, None)
        case whole :: part :: Nil => (whole, part.some)
        case whole :: Nil         => (whole, None)
        case _                    => throw new RuntimeException(s"WTF... cant format currency value: $value")

    val part: Option[String] = if tmpPart.isEmpty && forceDecimal then "0".some else tmpPart

    part match
      case Some(part) if includeCurrency => str"$$$whole.${part.alignLeft(currencyCode.scale, '0')} ${currencyCode.code}"
      case Some(part)                    => str"$$$whole.${part.alignLeft(currencyCode.scale, '0')}"
      case None if includeCurrency       => str"$$$whole ${currencyCode.code}"
      case None                          => str"$$$whole"
  }

  override def show: Text = show(true, false)

}
object UnpreciseMoney {

  def apply(value: MoneyValue, currencyCode: CurrencyCode): UnpreciseMoney =
    value match
      case MoneyValue.Unprecise(value)     => UnpreciseMoney(value, currencyCode)
      case MoneyValue.Precise(whole, part) => UnpreciseMoney(BigDecimal(whole) + BigDecimal(part) / currencyCode.multiplier, currencyCode)

}
