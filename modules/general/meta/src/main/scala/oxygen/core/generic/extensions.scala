package oxygen.core.generic

import oxygen.predef.base.*
import scala.annotation.targetName

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Show
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension (self: Show.type)
  inline def derived[A]: Show[A] =
    ShowGeneric.derived[A]

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      StrictEnum
//////////////////////////////////////////////////////////////////////////////////////////////////////

implicit class StrictEnumExtensions(self: StrictEnum.type) {

  @targetName("derive_nel")
  inline def derive[A: TypeTag](encode: A => NonEmptyList[String]): StrictEnum[A] = StrictEnumGeneric.derived[A](encode)

  @targetName("derive_single")
  inline def derive[A: TypeTag](encode: A => String): StrictEnum[A] = StrictEnumGeneric.derived[A](a => NonEmptyList.one(encode(a)))

  @targetName("derive")
  inline def derive[A: {TypeTag, EnumEncoding as enc}]: StrictEnum[A] = StrictEnumGeneric.derived[A](enc.encodeMany)

}

extension (self: StrictEnum.type)
  inline def derived[A: {TypeTag, EnumEncoding}]: StrictEnum[A] =
    StrictEnum.derive[A]

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      StrictEnum
//////////////////////////////////////////////////////////////////////////////////////////////////////

implicit class EnumWithOtherExtensions(self: EnumWithOther.type) {

  @targetName("derive_nel_wrap")
  inline def derive[A: TypeTag](encode: A => NonEmptyList[String], wrapOther: String => A): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](encode, wrapOther)

  @targetName("derive_single_wrap")
  inline def derive[A: TypeTag](encode: A => String, wrapOther: String => A): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](a => NonEmptyList.one(encode(a)), wrapOther)

  @targetName("derive_wrap")
  inline def derive[A: {TypeTag, EnumEncoding as enc}](wrapOther: String => A): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](enc.encodeMany, wrapOther)

  @targetName("derive_nel")
  inline def derive[A: TypeTag](encode: A => NonEmptyList[String]): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](encode)

  @targetName("derive_single")
  inline def derive[A: TypeTag](encode: A => String): EnumWithOther[A] = EnumWithOtherGeneric.derived[A](a => NonEmptyList.one(encode(a)))

  // TODO (KR) : this should have a different default, it should do _.toString if not the other case, otherwise get the wrapped value if its the other case
  @targetName("derive")
  inline def derive[A: {TypeTag, EnumEncoding as enc}]: EnumWithOther[A] = EnumWithOtherGeneric.derived[A](enc.encodeMany)

}

extension (self: EnumWithOther.type)
  inline def derived[A: {TypeTag, EnumEncoding}]: EnumWithOther[A] =
    EnumWithOther.derive[A]
