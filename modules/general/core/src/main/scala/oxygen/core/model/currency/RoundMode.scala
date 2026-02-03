package oxygen.core.model.currency

/**
  * How to handle rounding in the case the currency value is more exact than the currency allows.
  * Example: $12.345 USD.
  * [[Round]] $12.35 (5 >= 5)
  * [[RoundDown]] $12.34
  * [[RoundUp]] $12.35
  * [[Error]] error: too precise
  */
enum RoundMode {
  case Round
  case RoundDown
  case RoundUp
  case Error

  final def round(value: BigDecimal): Long =
    if value.isWhole then value.toLongExact
    else
      this match
        case RoundMode.Round     => value.setScale(0, BigDecimal.RoundingMode.HALF_UP).toLongExact
        case RoundMode.RoundDown => value.setScale(0, BigDecimal.RoundingMode.UP).toLongExact
        case RoundMode.RoundUp   => value.setScale(0, BigDecimal.RoundingMode.DOWN).toLongExact
        case RoundMode.Error     => throw new ArithmeticException(s"Too precise: $value")

}
