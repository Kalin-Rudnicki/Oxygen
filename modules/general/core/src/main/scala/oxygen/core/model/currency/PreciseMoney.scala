package oxygen.core.model.currency

import oxygen.core.Text
import oxygen.core.typeclass.Showable

final case class PreciseMoney private (positive: Boolean, unsignedFractionalUnits: Long, currencyCode: CurrencyCode) extends Showable {

  def toBigDecimal: BigDecimal = {
    val base: BigDecimal = BigDecimal(unsignedFractionalUnits) / currencyCode.multiplier
    if positive then base else -base
  }
  def unprecise: UnpreciseMoney = UnpreciseMoney(toBigDecimal, currencyCode)

  def unary_- : PreciseMoney = PreciseMoney(!positive, unsignedFractionalUnits, currencyCode)

  def add(by: MoneyValue, roundMode: RoundMode): PreciseMoney = this.unprecise.add(by).precise(roundMode)
  def add(by: MoneyValue): PreciseMoney = add(by, RoundMode.Error)

  def subtract(by: MoneyValue, roundMode: RoundMode): PreciseMoney = this.unprecise.subtract(by).precise(roundMode)
  def subtract(by: MoneyValue): PreciseMoney = subtract(by, RoundMode.Error)

  def times(by: BigDecimal, roundMode: RoundMode): PreciseMoney = this.unprecise.times(by).precise(roundMode)
  def times(by: Double, roundMode: RoundMode): PreciseMoney = times(BigDecimal(by), roundMode)
  def times(by: Long, roundMode: RoundMode): PreciseMoney = times(BigDecimal(by), roundMode)
  def times(by: BigDecimal): PreciseMoney = times(by, RoundMode.Round)
  def times(by: Double): PreciseMoney = times(by, RoundMode.Round)
  def times(by: Long): PreciseMoney = times(by, RoundMode.Round)

  def divide(by: BigDecimal, roundMode: RoundMode): PreciseMoney = this.unprecise.divide(by).precise(roundMode)
  def divide(by: Double, roundMode: RoundMode): PreciseMoney = divide(BigDecimal(by), roundMode)
  def divide(by: Long, roundMode: RoundMode): PreciseMoney = divide(BigDecimal(by), roundMode)
  def divide(by: BigDecimal): PreciseMoney = divide(by, RoundMode.Round)
  def divide(by: Double): PreciseMoney = divide(by, RoundMode.Round)
  def divide(by: Long): PreciseMoney = divide(by, RoundMode.Round)

  def show(includeCurrency: Boolean, forceDecimal: Boolean): Text = unprecise.show(includeCurrency, forceDecimal)
  override def show: Text = unprecise.show

}
object PreciseMoney {

  def apply(value: MoneyValue, currencyCode: CurrencyCode, roundMode: RoundMode = RoundMode.Error): PreciseMoney =
    UnpreciseMoney(value, currencyCode).precise(roundMode)

  def fromUnprecise(money: UnpreciseMoney, roundMode: RoundMode = RoundMode.Error): PreciseMoney = {
    val (positive, unsignedValue): (Boolean, BigDecimal) =
      if money.value >= 0 then (true, money.value)
      else (false, money.value.abs)

    val unsignedFractionalUnitDecimal: BigDecimal = unsignedValue * money.currencyCode.multiplier
    val unsignedFractionalUnitLong: Long = roundMode.round(unsignedFractionalUnitDecimal)
    PreciseMoney(positive, unsignedFractionalUnitLong, money.currencyCode)
  }

}
