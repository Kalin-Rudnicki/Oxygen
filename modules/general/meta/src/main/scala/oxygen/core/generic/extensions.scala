package oxygen.core.generic

import oxygen.predef.base.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Show
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension (self: Show.type)
  inline def derived[A]: Show[A] =
    ShowGeneric.derived[A]

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      StrictEnum
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension (self: StrictEnum.type) {

  inline def deriveNel[A: TypeTag](encode: A => NonEmptyList[String]): StrictEnum[A] = StrictEnumGeneric.derived[A](encode)

  inline def deriveSingle[A: TypeTag](encode: A => String): StrictEnum[A] = StrictEnumGeneric.derived[A](a => NonEmptyList.one(encode(a)))
  inline def derive[A: TypeTag](encode: A => String): StrictEnum[A] = StrictEnumGeneric.derived[A](a => NonEmptyList.one(encode(a)))

  inline def derive[A: {TypeTag, EnumEncoding as enc}]: StrictEnum[A] = StrictEnumGeneric.derived[A](enc.encodeMany)

}

extension (self: StrictEnum.type)
  inline def derived[A: {TypeTag, EnumEncoding}]: StrictEnum[A] =
    StrictEnum.derive[A]

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      StrictEnum
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension (self: EnumWithOther.type) {

  inline def deriveNelWrap[A: TypeTag](encode: A => NonEmptyList[String], wrapOther: String => A): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](encode, wrapOther)

  inline def deriveSingleWrap[A: TypeTag](encode: A => String, wrapOther: String => A): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](a => NonEmptyList.one(encode(a)), wrapOther)

  inline def deriveWrap[A: {TypeTag, EnumEncoding as enc}](wrapOther: String => A): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](enc.encodeMany, wrapOther)

  inline def deriveNel[A: TypeTag](encode: A => NonEmptyList[String]): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](encode)

  inline def deriveSingle[A: TypeTag](encode: A => String): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](a => NonEmptyList.one(encode(a)))
  inline def derive[A: TypeTag](encode: A => String): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](a => NonEmptyList.one(encode(a)))

  inline def derive[A: {TypeTag, EnumEncoding as enc}]: EnumWithOther[A] = EnumWithOtherGeneric.derived[A](enc.encodeMany)

}

extension (self: EnumWithOther.type)
  inline def derived[A: {TypeTag, EnumEncoding}]: EnumWithOther[A] =
    EnumWithOther.derive[A]
