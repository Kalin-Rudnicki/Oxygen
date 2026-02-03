package oxygen.core.model.currency

sealed trait MoneyValue
object MoneyValue {

  final case class Precise(whole: Long, part: Long) extends MoneyValue
  final case class Unprecise(value: BigDecimal) extends MoneyValue

  def whole(w: Long, p: Long = 0): MoneyValue = Precise(w, p)
  def part(p: Long): MoneyValue = Precise(0, p)
  def apply(value: BigDecimal): MoneyValue = Unprecise(value)
  def apply(value: Double): MoneyValue = Unprecise(BigDecimal(value))
  def apply(value: String): MoneyValue = Unprecise(BigDecimal(value))

  given Conversion[Long, MoneyValue] = MoneyValue.whole(_)
  given Conversion[Int, MoneyValue] = MoneyValue.whole(_)
  given Conversion[BigDecimal, MoneyValue] = MoneyValue(_)
  given Conversion[Double, MoneyValue] = MoneyValue(_)
  given Conversion[String, MoneyValue] = MoneyValue(_)

}
