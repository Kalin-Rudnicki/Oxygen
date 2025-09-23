package oxygen.quoted

import scala.quoted.*

sealed abstract class TypeType(final val isScala2: Boolean, final val isEnum: Boolean) {

  final def toSealed: Option[TypeType.Sealed] = this match
    case self: TypeType.Sealed => Some(self)
    case _: TypeType.Case      => None

  final def toCase: Option[TypeType.Case] = this match
    case self: TypeType.Case => Some(self)
    case _: TypeType.Sealed  => None

  final def toCaseClass: Option[TypeType.Case.Class] = this match
    case self: TypeType.Case.Class => Some(self)
    case _                         => None

  final def toCaseObject: Option[TypeType.Case.Object] = this match
    case self: TypeType.Case.Object => Some(self)
    case _                          => None

  final def baseType: String = this match
    case _: TypeType.Case   => "product type"
    case _: TypeType.Sealed => "sum type"

}
object TypeType {

  sealed abstract class Sealed(isScala2: Boolean, isEnum: Boolean) extends TypeType(isScala2, isEnum)
  object Sealed {
    def fromSym(sym: Symbol)(using Quotes): Option[TypeType.Sealed] = TypeType.fromSym(sym).flatMap(_.toSealed)
  }

  sealed abstract class Case(isScala2: Boolean, isEnum: Boolean, final val isObject: Boolean) extends TypeType(isScala2, isEnum)
  object Case {

    sealed abstract class Class(isScala2: Boolean, isEnum: Boolean) extends Case(isScala2, isEnum, false)
    object Class {
      def fromSym(sym: Symbol)(using Quotes): Option[TypeType.Case.Class] = TypeType.fromSym(sym).flatMap(_.toCaseClass)
    }

    sealed abstract class Object(isScala2: Boolean, isEnum: Boolean) extends Case(isScala2, isEnum, true)
    object Object {
      def fromSym(sym: Symbol)(using Quotes): Option[TypeType.Case.Object] = TypeType.fromSym(sym).flatMap(_.toCaseObject)
    }

    def fromSym(sym: Symbol)(using Quotes): Option[TypeType.Case] = TypeType.fromSym(sym).flatMap(_.toCase)

  }

  case object SealedTrait extends TypeType.Sealed(false, false)
  case object SealedAbstractClass extends TypeType.Sealed(false, false)
  case object SealedEnum extends TypeType.Sealed(false, true)
  case object Scala2SealedTrait extends TypeType.Sealed(true, false)
  case object Scala2SealedAbstractClass extends TypeType.Sealed(true, false)

  case object CaseClass extends TypeType.Case.Class(false, false)
  case object EnumCaseClass extends TypeType.Case.Class(false, true)
  case object Scala2CaseClass extends TypeType.Case.Class(true, false)

  case object CaseObject extends TypeType.Case.Object(false, false)
  case object EnumCaseObject extends TypeType.Case.Object(false, true)
  case object Scala2CaseObject extends TypeType.Case.Object(true, false)

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
