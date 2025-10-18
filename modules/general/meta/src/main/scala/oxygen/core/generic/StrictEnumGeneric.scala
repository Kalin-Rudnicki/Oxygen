package oxygen.core.generic

import oxygen.meta.*
import oxygen.predef.base.*

object StrictEnumGeneric {

  inline def derived[A: TypeTag](encode: A => NonEmptyList[String]): StrictEnum[A] = {
    val values: Seq[A] = K0.SumGeneric.EnumGeneric.deriveEnum.strictEnum.values[A]
    StrictEnum.make[A](values, encode)
  }

}
