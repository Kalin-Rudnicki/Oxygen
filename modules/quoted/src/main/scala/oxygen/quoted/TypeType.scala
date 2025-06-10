package oxygen.quoted

import scala.quoted.*

sealed abstract class TypeType(final val isScala2: Boolean, final val isEnum: Boolean) {

  final def toSealed: Option[TypeType.Sealed] = this match
    case self: TypeType.Sealed => Some(self)
    case _: TypeType.Case      => None

  final def toCase: Option[TypeType.Case] = this match
    case self: TypeType.Case => Some(self)
    case _: TypeType.Sealed  => None

}
object TypeType {

  sealed abstract class Sealed(isScala2: Boolean, isEnum: Boolean) extends TypeType(isScala2, isEnum)
  object Sealed {
    def fromSym(sym: Symbol)(using Quotes): Option[TypeType.Sealed] = TypeType.fromSym(sym).flatMap(_.toSealed)
  }

  sealed abstract class Case(isScala2: Boolean, isEnum: Boolean, final val isObject: Boolean) extends TypeType(isScala2, isEnum)
  object Case {
    def fromSym(sym: Symbol)(using Quotes): Option[TypeType.Case] = TypeType.fromSym(sym).flatMap(_.toCase)
  }

  case object SealedTrait extends TypeType.Sealed(false, false)
  case object SealedAbstractClass extends TypeType.Sealed(false, false)
  case object SealedEnum extends TypeType.Sealed(false, true)
  case object Scala2SealedTrait extends TypeType.Sealed(true, false)
  case object Scala2SealedAbstractClass extends TypeType.Sealed(true, false)

  case object CaseClass extends TypeType.Case(false, false, false)
  case object CaseObject extends TypeType.Case(false, false, true)
  case object EnumCaseClass extends TypeType.Case(false, true, false)
  case object EnumCaseObject extends TypeType.Case(false, true, true)
  case object Scala2CaseClass extends TypeType.Case(true, false, false)
  case object Scala2CaseObject extends TypeType.Case(true, false, true)

  def fromSym(sym: Symbol)(using Quotes): Option[TypeType] = {
    val flags = sym.flags

    (
      flags.is(Flags.Scala2x),
      flags.is(Flags.Enum),
      flags.is(Flags.Case),
      flags.is(Flags.Sealed),
      flags.is(Flags.Trait),
      // flags.is(Flags.NoInits),
      flags.is(Flags.StableRealizable),
      flags.is(Flags.Module),
    ) match {
      // scala 3
      case (false, false, true, false, false, false, false) => Some(CaseClass)
      case (false, false, true, false, false, true, true)   => Some(CaseObject)
      case (false, true, true, false, false, false, false)  => Some(EnumCaseClass)
      case (false, true, true, false, false, true, false)   => Some(EnumCaseObject)
      case (false, false, false, true, true, false, false)  => Some(SealedTrait)
      case (false, false, false, true, false, false, false) => Some(SealedAbstractClass)
      case (false, true, false, true, false, false, false)  => Some(SealedEnum)

      // scala 2
      case (true, false, true, false, false, false, false) => Some(Scala2CaseClass)
      case (true, false, true, false, false, false, true)  => Some(Scala2CaseObject)
      case (true, false, false, true, false, false, false) => Some(Scala2SealedAbstractClass)
      case (true, false, false, true, true, false, false)  => Some(Scala2SealedTrait)

      case _ => None
    }
  }

}
