package oxygen.core.generic

import oxygen.meta.*
import oxygen.predef.base.*

object EnumWithOtherGeneric {

  inline def derived[A: TypeTag](encode: A => NonEmptyList[String], wrapOther: String => A): EnumWithOther[A] = {
    val values: Seq[A] = K0.SumGeneric.EnumGeneric.deriveEnum.ignoreSingleCaseClass.values[A]
    EnumWithOther.make[A](values, encode, wrapOther)
  }

  inline def derived[A: TypeTag](encode: A => NonEmptyList[String]): EnumWithOther[A] = {
    val (values: Seq[A], wrapOther: (String => A)) = K0.SumGeneric.EnumGeneric.deriveEnum.ignoreSingleCaseClass.valuesAndWrap[A, String]
    EnumWithOther.make[A](values, encode, wrapOther)
  }

}
