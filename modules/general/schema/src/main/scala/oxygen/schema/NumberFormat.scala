package oxygen.schema

import oxygen.predef.core.*

sealed trait NumberFormat derives StrictEnum
object NumberFormat {

  // TODO (KR) : some repr for max/min size, and how much decimal precision
  //           : does BigDecimal have a sliding scale of how much precision it has depending on how big the whole is?
  //           : ex: `10000000000.1` vs `1.00000000001`

  sealed trait Whole extends NumberFormat
  case object BigInt extends Whole
  case object Int64 extends Whole
  case object Int32 extends Whole
  case object Int16 extends Whole
  case object Int8 extends Whole

  sealed trait Fractional extends NumberFormat
  case object BigDecimal extends Fractional
  case object Float64 extends Fractional
  case object Float32 extends Fractional

}
